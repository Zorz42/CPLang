#include<stdio.h>

typedef struct cplang_T0 {
    int x;
} cplang_T0;
void cplang_main0() {
cplang_T0 a = (cplang_T0){10};
int b = (*(&a)).x;
b = 20;
printf("%d\n", (a).x);
}

int main(){cplang_main0();return 0;}

