#include <stdio.h>
#include <stdlib.h>
int z_order2d_fortran(int *x, int *y);

int main(){
    int n;
    
  
    printf("Enter number (must be one of 2, 4, 8, or 16):\n");
    scanf("%d", &n);
    if (n != 2){
        if (n != 4){
            if (n != 8){
                if (n != 16){
                    printf("n must be one of 2, 4, 8, or 16 \n");
                    exit(1);
                }
            }
        }
    }
    
    int arr[n][n];
    for (int i = 0; i < n; i++){
        for (int j = 0; j < n; j++){
            int n = i + 1;
            int m = j + 1;
            arr[i][j] = z_order2d_fortran(&m, &n);
        }
    }

    
    for (int i = 0; i < n; i++){ 
        for (int j = 0; j < n; j++){
            printf("%d ", arr[i][j]);
        }
        printf("\n");
    }

    return 0;
}
