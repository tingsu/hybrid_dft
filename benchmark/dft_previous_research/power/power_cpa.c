#include<stdio.h>

//extern function for cpachecker
extern int __VERIFIER_nondet_int();

double power(int x, int y) {

  int exp;
  double res;

  if (y > 0) 
    exp = y; 
  else 
    exp = -y;

  res = 1;

  while (exp != 0) {
    
    res *= x; 
    exp -= 1;
  }

  if (y <= 0)
    if (x == 0) 
      return -1;
    else {
      return 1.0 / res; 
    }
  else {
    res = res * 1.0;
    return res; 
  }

  return 0;
}

void testme(){

    int x;
    int y;
    x = __VERIFIER_nondet_int();
    y = __VERIFIER_nondet_int();
    power(x,y);
    
}



