#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "mmio.h"

//Structure for holding index and values 
struct Element{
    int ind;
    int count;
};
//Stucture to sort row, col, val by row num
struct row_col_val{
	int row_ind;
	int col_ind;
	double value;
};

//comparison for row_col_val
int compare_row(const void *a, const void *b){
	struct row_col_val *rowA = (struct row_col_val *)a;
	struct row_col_val *rowB = (struct row_col_val *)b;
	return (rowA -> row_ind - rowB -> row_ind);
};


//comparison function for qsort
int compare_count(const void *a, const void *b){
    struct Element *elemA = (struct Element *)a;
    struct Element *elemB = (struct Element *)b;
    return (elemB -> count - elemA -> count);
}


int main(int argc, char *argv[]) {
    int ret_code;
    MM_typecode matcode;
    FILE *f;
    int M, N, nz;
    int *I, *J;
    double *val;

    if (argc != 4){
        printf("3 arguments required <spfmat> <nmults> <mmfile> \n");
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
    
   
    //make a random vector of size M to perform the matrix multiplication
    double vec[M];
    double result[M];
    for(int i = 0; i < M; i++){
        int numer = rand() % 10000;
        int denom = rand() % 10000;
        vec[i] = numer/denom;
        result[i] = 0;
    }


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
        clock_t start = clock();
		for(int k=0; k<nmults; k++){ 
        	for(int i=0; i<M; i++){
            	for(int j=0; j<N; j++){
                	result[i] += A[i][j]*vec[j]; 
            	}
        	}
			for(int i = 0; i < M; i++){
				vec[i] = result[i];
			}
		}
        clock_t end = clock();
        double time_spent = (double)(end-start)/CLOCKS_PER_SEC;
        for(int i=0; i < M; i++){
            printf("%f \n", result[i]);
        }
        printf("time spent: %f \n", time_spent);

    }else if (strcmp(spfmat, "COO") == 0){
        //mats read in in coo
        clock_t start = clock();
		for(int j = 0; j < nmults; j++){
        	for(int i = 0; i < nz; i++){
            	result[I[i]] += val[i]*vec[J[i]];	
			}
			for(int i=0; i<M; i++){
				vec[i] = result[i];
			}	
		}
        clock_t end = clock();
        double time_spent = (double)(end-start)/CLOCKS_PER_SEC;
        for(int i=0; i < M; i++){
            printf("%f \n", result[i]);
        }
        printf("time spent: %f \n", time_spent);

    }else if (strcmp(spfmat, "CSR") == 0){
       //init arrays 
	   int col_idx[nz];
	   double val_idx[nz];
       int row_idx[nz];
	   int row_count[M];
	   int row_ptr[M+1];
       //set num nonzero elements in each row to zero
       for(int i = 0; i < M; i++){
           row_count[i] = 0;
       }
		struct row_col_val indexs[nz];
		//fill row_col_val
		for(int i = 0; i < nz; i++){
			indexs[i].row_ind = I[i];
			indexs[i].col_ind = J[i];
			indexs[i].value = val[i];
		}
		//sort based on rows
		qsort(indexs, nz, sizeof(struct row_col_val), compare_row);
		for (int i = 0; i < nz; i++){
			row_idx[i] = indexs[i].row_ind;
			col_idx[i] = indexs[i].col_ind;
			val_idx[i] = indexs[i].value;
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
		//do matvec
        clock_t start = clock();
		for(int k =0; k < nmults; k++){
	   		int val_index = 0;
	   		for(int i = 0; i < M+1; i++){
				for(int j = 0; j < row_ptr[i+1] - row_ptr[i]; j++){
					int temp_ind = col_idx[val_index];
			    	result[i] += val_idx[val_index]*vec[temp_ind];	
					val_index +=  1;
				}
			}
			for(int i = 0; i < M; i++){
				vec[i] = result[i];
			}
		}
        clock_t end = clock();
        double time_spent = (double)(end-start)/CLOCKS_PER_SEC;
		//print result
		for(int i = 0; i < M; i++){
	 	    printf("%f", result[i]);
			printf("\n");
		}
        printf("time spent: %f \n", time_spent);
             
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
		//do matvec
        clock_t start = clock();
		for(int k = 0; k<nmults; k++){
			for(int i = 0; i < M*max; i++){
				int temp_col = flat_col[i];
				int temp_row = i % M;
				double temp_val = flat_val[i];
				if(temp_col != -1){
					result[temp_row] += temp_val*vec[temp_col];
				}
			}
			for(int i = 0; i < M; i++){
				vec[i] = result[i];
			}
		}         
        clock_t end = clock();
        double time_spent = (double)(end-start)/CLOCKS_PER_SEC;
		//print result
        for(int i = 0; i < M; i++){
   			printf("%f", result[i]);
			printf("\n");
		}
        printf("time spent: %f \n", time_spent);
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
        int* jagged_cols[M];
        double* jagged_vals[M];
     
        for(int i = 0; i < M; i++){
	   		int tmp_size = row_ind[i].count; 
            jagged_cols[i] = (int*)malloc(tmp_size * sizeof(int));
            jagged_vals[i] = (double*)malloc(tmp_size * sizeof(double));
   
			    
		
           if (jagged_cols[i] == NULL || jagged_vals[i] == NULL){
                printf("failed to allocate column memory");
                for(int j = 0; j < i; j++){
                    free(jagged_cols[i]);
                    free(jagged_vals[i]);
                }
                return 2;
           }
		}      	   

        //move to intermediate array 
        int new_rows_ind[M];
        for(int i = 0; i < M; i++){
            new_rows_ind[i] = row_ind[i].ind;
            row_count[i] = 0;
           
        }


        //load data
        for(int i = 0; i < nz; i++){
            int row_num;
            for(int j = 0; j<M; j++){
                if(new_rows_ind[j] == I[i]){
                    row_num = j;
                    break;
                }
            }            
            jagged_cols[row_num][row_count[row_num]] = J[i];
            jagged_vals[row_num][row_count[row_num]] = val[i];
            row_count[row_num] ++;
        }

		//count elements in each col
		int col_count[row_ind[0].count];
		for(int i = 0; i < row_ind[0].count; i++){
			col_count[i] = 0;
		}
		for(int i = 0; i < M; i++){
			for(int j = 0; j < row_ind[0].count; j++){
				if(j < row_ind[i].count){
					col_count[j] ++;
				}
			}
		}
		//make iterator
		int iter[row_ind[0].count + 1];
		for(int i = 0; i < row_ind[0].count+1; i++){
			if(i == 0){
				iter[i] = 0;
			}else{
				iter[i] = iter[i-1] + col_count[i-1];	
			}
		}
		
		//flatten array	
		int flattened_col[nz];
		double flattened_val[nz];
        for(int i = 0; i < nz; i++){
            flattened_col[i] = 0;
            flattened_val[i] = 0;
        }
		int flat_idx = 0;
		for(int i = 0; i < row_ind[0].count; i++){
			for(int j = 0; j < col_count[i]; j++){
				flattened_col[flat_idx] = jagged_cols[j][i];
				flattened_val[flat_idx] = jagged_vals[j][i];
				flat_idx ++;
                
			}
		}
        int col_val_idx;
        int vec_idx;
        int result_idx;
        int matvec_ind;
        int temp_one;
        int temp_two;
        int temp_counter = row_ind[0].count;
        clock_t start = clock();
		for(int k = 0; k<nmults; k++){
			col_val_idx = 0;
			vec_idx=0;
			result_idx=0;
			for(int i = 0; i < temp_counter; i++){
                temp_one = iter[i+1];
                temp_two = iter[i];
                matvec_ind = temp_one - temp_two;
				for(int j = 0; j < matvec_ind; j++){
					vec_idx = flattened_col[col_val_idx];
					result_idx = row_ind[j].ind;
					result[result_idx] += flattened_val[col_val_idx]*vec[vec_idx];
					col_val_idx += 1;
				}	
			}	
			for(int i = 0; i<M; i++){
				vec[i] = result[i];
			}
		}
        clock_t end = clock();
        double time_spent = (double)(end-start)/CLOCKS_PER_SEC;
		for(int i = 0; i < M; i++){
			printf("%f \n", result[i]);
		}
        printf("time spent: %f \n", time_spent);
		for(int i = 0; i < M; i++){
			free(jagged_vals[i]);
			free(jagged_cols[i]);
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
