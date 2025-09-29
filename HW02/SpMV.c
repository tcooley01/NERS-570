#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mmio.h"

//Structure for holding index and values 
struct Element{
    int ind;
    int count;
};

//comparison function for qsort
int compare_count(const void *a, const void *b){
    struct Element *elemA = (struct Element *)a;
    struct Element *elemB = (struct Element *)b;
    return (elemB -> count - elemA -> count);
}

//function to find first index of a value
int find_index(int arr[], int arr_size, int val){
    for(int i = 0; arr_size; i++){
        if(arr[i] == val){
            return i;
        }
    }
    
    printf("failed to find index in the array");
    return -1;    
}

int main(int argc, char *argv[]) {
    int ret_code;
    MM_typecode matcode;
    FILE *f;
    int M, N, nz;
    int *I, *J;
    double *val;

    if (argc != 6){
        printf("5 arguments required <spfmat> <nmults> <mmfile> <vecfilein> <vecfileout> \n");
        exit(1);        
    }
    
    if((f = fopen(argv[3], "r")) == NULL){
        exit(1);
    }
    if (mm_read_banner(f, &matcode) != 0){
        printf("Could not process Matrix Market banner.\n");
        exit(1); 
    }

    //find size of sparse matrix
    if ((ret_code = mm_read_mtx_crd_size(f, &M, &N, &nz)) != 0)
        exit(1);

    //reserve memory for matricies
    I = (int *) malloc(nz*sizeof(int));
    J = (int *) malloc(nz*sizeof(int));
    val = (double *) malloc(nz * sizeof(double));

   for (int i=0; i<nz; i++){
       fscanf(f, "%d %d %lg\n", &I[i], &J[i], &val[i]);
        I[i]--;
        J[i]--;
    }
    if (f != stdin) fclose(f);

    

    char* spfmat = argv[1];
    int nmults = atoi(argv[2]);
    char* mmfile = argv[3];
    char* vecfilein = argv[4];
    char* vecfileout = argv[5];

    //write mat
    //mm_write_banner(stdout, matcode);
    //mm_write_mtx_crd_size(stdout, M, N, nz);
    //for(int i=0; i<nz; i++){
    //    fprintf(stdout, "%d %d %20.19g\n", I[i]+1, J[i]+1, val[i]);
    //}

    if (strcmp(spfmat, "DEN") == 0){
        //init A matriz and fill with zeros
        double A[M][N];
        for(int i = 0; i<M; i++){
            for(int j = 0; j<N; j++){
                A[i][j] = 0;
            }
        }
        
        //fill A matrix with data
        for(int i = 0; i < nz; i++){
            A[I[i]][J[i]] = val[i];
        }
        
        
    }else if (strcmp(spfmat, "COO") == 0){
        //mats read in in coo
    }else if (strcmp(spfmat, "CSR") == 0){
       //init arrays 
       int row_count[M]; 
       int row_ptr[M+1];

       //set num nonzero elements in each row to zero
       for(int i = 0; i < M; i++){
           row_count[i] = 0;
       }

       //count the number of nonzero elements in the array
       for(int i = 0; i < nz; i++){
           row_count[I[i]] += 1;
       } 

       //calculate row ptrs
       for(int i = 0; i <= M; i++){
           if(i == 0){
               row_ptr[i] = 0;
           }else{
               row_ptr[i] = row_ptr[i-1] + row_count[i-1];
           }
       }
       
    }else if (strcmp(spfmat, "ELL") == 0){
        //count num non zero observations in each row
        int row_count[M];
        for(int i = 0; i < M; i++){
            row_count[i] = 0;
        }
        for(int i = 0; i < nz; i++){
            row_count[I[i]] += 1;
        }
        
        //Find max number of obs in a row
        int max = 0;
        for(int i = 0; i < M; i++){
            if(row_count[i] > max){
                max = row_count[i];
            }
        }
       
        //Init two arrays one for col indexes one for vals 
        //in array with col indexes use -1 as padding element since and idex shouldnt be -1 and 0 for value as if a value is zero we are not storing it
        int col_array[M][max];
        double val_array[M][max];
        for(int i = 0; i < M; i++){
            for(int j = 0; j < max; j++){
                col_array[i][j] = -1;
                val_array[i][j] = 0;
            }
            row_count[i] = 0;
        }
             
        //fill arrays with nonzero data
        for(int i = 0; i < nz; i++){
            int ind = I[i];
            col_array[ind][row_count[ind]] = J[i];
            val_array[ind][row_count[ind]] = val[i];
            row_count[ind] += 1;
        }
        //flatten in column major order
        int flat_col[M*max];
        double flat_val[M*max];
        
        for(int i = 0; i < max; i++){
            for(int j = 0; j < M; j++){
                flat_col[M*i + j] = col_array[j][i];
                flat_val[M*i + j] = val_array[j][i];
            }
        }         
        for(int i = 0; i < M*max; i++){
            printf("%d, %f \n", flat_col[i], flat_val[i]);
        }
    }else if (strcmp(spfmat, "JDS") == 0){
        //init structure to keep track of row ind and sort by num nonzero obs
        int row_count[M];
        struct Element row_ind[M];

        //count num nonzero in each row
        for(int i = 0; i < M; i++){
            row_count[i] = 0;
        }
        for(int i = 0; i < nz; i++){
            row_count[I[i]] += 1;
        }
        //fill row count array with structs of row counts and inds
        for(int i = 0; i < M; i++){
            row_ind[i].count = row_count[i];
            row_ind[i].ind = i;
        }
        //sort by number of nonzero obs
        qsort(row_ind, M, sizeof(struct Element), compare_count);

        //init jagged matrix of numrows by num nonzero obs in each row
        int **jagged_cols = (int **)malloc(M * sizeof(int));
        double **jagged_vals = (double **)malloc(M * sizeof(double));
        if(jagged_cols == NULL || jagged_vals == NULL){
            printf("failed to allocate row memory");
            return 2;
        }
        for(int i = 0; i < M; i++){
            jagged_cols[i] = (int*)malloc(row_ind[i].count * sizeof(int));
            jagged_vals[i] = (double*)malloc(row_ind[i].count * sizeof(double));
            

            if (jagged_cols[i] == NULL || jagged_vals[i] == NULL){
                printf("failed to allocate column memory");
                for(int j = 0; j < i; j++){
                    free(jagged_cols[i]);
                    free(jagged_vals[i]);
                }
                free(jagged_cols);
                free(jagged_vals);
                return 2;
            }
        }

        //move to intermediate array 
        int new_rows_ind[M];
        for(int i = 0; i < M; i++){
            new_rows_ind[i] = row_ind[i].ind;
            row_count[i] = 0;
            printf("%d \n", new_rows_ind[i]);
        }

        for(int i = 0; i < M; i++){
            for(int j = 0; j < row_ind[i].count; j++){
                jagged_cols[i][j] = -1;
                jagged_vals[i][j] = 0;
            }
        }

        for(int i = 0; i < M; i++){
            printf("%d, ", row_count[i]);
            for(int j = 0; j < row_ind[i].count; j++){
                printf("%d,", jagged_cols[i][j]);
            }
            printf("\n");
        }


        //load data
        for(int i = 0; i < nz; i++){
            int row_num = find_index(new_rows_ind, M, I[i]);
            printf("og idx: %d, rownum %d, colnum %d, colid %d, ", I[i], row_num, row_count[row_num], J[i]);
            jagged_cols[row_num][row_count[row_num]] = J[i];
            printf("what was written in %d \n", jagged_cols[row_num][row_count[row_num]]);
            jagged_vals[row_num][row_count[row_num]] = val[i];
            row_count[row_num] += 1;
        }

        for (int i = 0; i < M; i++){
            for(int j = 0; j < row_count[i]; j++){
                printf("%d, ", jagged_cols[i][j]);
            }
            printf("\n");
        }
        for (int i = 0; i < M; i++){
            for(int j = 0; j < row_count[i]; j++){
                printf("%f, ", jagged_vals[i][j]);
            }
            printf("\n");
        }


    }else{
       printf("the provided format is unrecogenized must be one of: Dense (DEN),coordinate formate (COO),compressed sparse row (CSR), ELLPack (ELL) or jagged diagonal storage (JDS)\n");
       return 3;
    }
   
    free(I);
    free(J);
    free(val);
        
    return 0;
}
