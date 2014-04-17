#include <stdio.h>

double x;

int globmagic(void) {
   x=x - 5.0;
   return 0; 
}

int main(void) {
    x = 6.0;
    if((globmagic()) + x > globmagic()) {
        printf("x > 0 %lf\n", x);
    }
    else printf("x < 0 %lf\n", x);
    return 0;
}
