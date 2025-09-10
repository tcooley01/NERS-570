#include <stdio.h>

int *z_order2d(int x, int y){
    
    //Compute x coord
    x = (x | (x << 8)); 
    x = (x | (x << 4));
    x = (x | (x << 2));
    x = (x | (x << 1));

    //Compute y cord
    y = (y | (y << 8));
    y = (y | (y << 4));
    y = (y | (y << 2));
    y = (y | (y << 1));    
    int *arr = malloc(2)
    arr[0] = x;
    arr[1] = y;

    return arr; 
}

int main(){

    printf("Please enter an array size which must be 2, 4, 8, or 16");

    int *arr = z_order2d(1, 2);
    if (arr){ 
        printf("%d %d", arr[0], arr[1]);
        free(arr);
    }
    return 0;
}
