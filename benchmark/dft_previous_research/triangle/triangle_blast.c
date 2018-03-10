#include <stdio.h>

//extern function for cbmc
extern int nondet_int();

int triangle (int i, int j, int k) {
    // returns one of the following:
    // 1 if triangle is scalene
    // 2 if triangle is isosceles
    // 3 if triangle is equilateral
    // 4 if not a triangle
    if ( (i <= 0) || (j <= 0) || (k <= 0) ) return 4; // acd
    int tri = 0;
    if (i == j) tri += 1; // g
    if (i == k) tri += 2; // h
    if (j == k) tri += 3; // i
    if (tri == 0) { // bef
        if ( (i+j <= k) || (j+k <= i) || (i+k <= j) ) tri = 4; // be
        else tri = 1; // f
        return tri;
    }
    if (tri > 3) tri = 3;
    else if ( (tri == 1) && (i+j > k) ) tri = 2;
    else if ( (tri == 2) && (i+k > j) ) tri = 2; // h
    else if ( (tri == 3) && (j+k > i) ) tri = 2;
    else tri = 4;
    return tri;
}


void testme() {
    printf("enter 3 integers for sides of triangles\n");
    int a,b,c;
    a = nondet_int();
    b = nondet_int();
    c = nondet_int();
    int t = triangle(a,b,c);
    if (t == 1) printf("triangle is scalene\n"); // f
    else if (t == 2) printf("triangle is isosceles\n"); // h
    else if (t == 3) printf("triangle is equilateral\n");
    else if ( t== 4) printf("this is not a triangle\n"); // abcdegi
}

