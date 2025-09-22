#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    if (argc != 6){
        printf("5 arguments required <spfmat> <nmults> <mmfile> <vecfilein> <vecfileout> \n");
        return 1;        
    }
    char* spfmat = argv[1];
    int nmults = atoi(argv[2]);
    char* mmfile = argv[3];
    char* vecfilein = argv[4];
    char* vecfileout = argv[5];

    if (*spfmat == "DEN"){
    }else if (*spfmat == "COO"){
    }else if (*spfmat == "CSR"){
    }else if (*spfmat == "ELL"){
    }else if (*spfmat == "JDS"){
    }else{
       printf("the provided format is unrecogenized must be one of: Dense (DEN),coordinate formate (COO),compressed sparse row (CSR), ELLPack (ELL) or jagged diagonal storage (JDS)\n");
       return 2;
    
}
