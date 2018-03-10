//This program is based on the Pascal version of Hoare's find [14] program used by
//Frankl and Weiss [10]. FIND takes two inputs, an array a and an index f , and permutes
//the elements of a so that elements to the right of position f are greater than or equal to
//a[f] and elements to the left of position f are less than or equal to a[f].

#include <stdio.h>
#define max 5
typedef enum boolean {false, true} BOOLEAN;
int a[max + 1];  //input: a
int n, f; //input n, f

//extern function for cpachecker
extern int __VERIFIER_nondet_int();

/*
main()
{
    char *mystr;
    int i;
    char *gets();
    void find();
    mystr = (char *) malloc (80);
    scanf("%d", &n);   //input: n
    gets(mystr);
    scanf("%d", &f);   //input: f
    gets(mystr);
    for (i=1; i<=n; i++)
    { scanf("%d", &a[i]);
        gets(mystr);
    }
    printf("---\n");
    find(n, f);
    printf("---\n");
    printf("%5d\n", n);
    printf("%5d\n", f);
    for (i=1; i<=n; i++)
        printf("%5d\n", a[i]);
}
*/

void find(n, f)
int n;
int f;
{
    int m, ns, i, j, w;
    BOOLEAN b;
    b = false;
    m = 1;
    ns = n;

    while ((m < ns) || b)
    {
        if (!b)
        {
            i = m;
            j = ns;
        }
        else
            b = false;
            if (i > j)
            {
                if (f > j)
                {
                    if (i > f)
                        m = ns;
                    else
                        m = i;
                }
                else
                        ns = j;
            }
            else
            {
                while (a[i] < a[f])
                    i = i + 1 ;
                while (a[f] < a[j])
                    j = j - 1 ;
                if (i <= j)
                {
                    w = a[i];
                    a[i] = a[j];
                    a[j] = w;
                    i = i + 1;
                    j = j - 1;
                };
                b = true;
            }
    }
}

void testme(){

   int i;
  
   n= __VERIFIER_nondet_int();

   f= __VERIFIER_nondet_int();
  
   if(n<1 || f<1 || f>n || n>max) // precondition: 1<=f<=n<=max, annotated by Ting Su.
	return;

   for (i=1; i<=n; i++)
    { 
	a[i] = __VERIFIER_nondet_int();
    }
   
   find(n, f);
}
