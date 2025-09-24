#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mmio.h"

int main(int argc, char *argv[]) {
    int ret_code;
    MM_typecode matcode;
    FILE *f;
    int M, N, nz;
    int i, *I, *J;
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

   for (i=0; i<nz; i++){
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
    //for(i=0; i<nz; i++){
        //fprintf(stdout, "%d %d %20.19g\n", I[i]+1, J[i]+1, val[i]);
    //}

    if (strcmp(spfmat, "DEN") == 1){

    }else if (strcmp(spfmat, "COO") == 1){
    }else if (strcmp(spfmat, "CSR") == 1){
    }else if (strcmp(spfmat, "ELL") == 1){
    }else if (strcmp(spfmat, "JDS") == 1){
    }else{
       printf("the provided format is unrecogenized must be one of: Dense (DEN),coordinate formate (COO),compressed sparse row (CSR), ELLPack (ELL) or jagged diagonal storage (JDS)\n");
       return 2;
    }
   
    free(I);
    free(J);
    free(val);
        
    return 0;
}
