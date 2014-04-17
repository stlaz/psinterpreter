#include <stdio.h>

int x;

int sideeffector(int z) {
    x = z+x+1;
    return z+x+3;
}

int main(void) {
    x = 3;
    x = sideeffector(5) + sideeffector(7)*x;
    printf("%d\n", x);
    sideeffector(4) + sideeffector(2);
    printf("%d\n", x);
    return 0;
}
