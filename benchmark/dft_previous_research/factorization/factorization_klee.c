#include <stdio.h>
#include <klee/klee.h>
//extern function for cbmc
extern int nondet_int();

void factorization(int N){
    if (N>0) {
        int prime =2;
        int number = N;
        while (prime <= N/2) {
            if (number % prime == 0) {
                printf("%d\n", prime);
                number = number / prime;
            }
            else
            {
                int nextPrime = prime;
                int found = 0;
                while (found == 0) {
                    nextPrime = nextPrime + 1;
                    found = 1;
                    int d = 2;
                    while (d<= nextPrime/2) {
                        if (nextPrime %d == 0) {
                            found = 0;
                        }
                        d ++;
                    }
                }
                prime = nextPrime;
            }
        }
        if (number > 1) {
            printf("%d\n", number);
        }
    }
}

void testme(){
   int N;
   klee_make_symbolic(&N, sizeof(N), "N");
   factorization(N);
}
int main(){
    testme();
    return 0;
}
