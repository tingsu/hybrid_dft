#include <stdio.h>
#define tmax 80
#define pmax 3
extern char text[];  //input
extern char pattern[]; //input

//extern function for cpachecker
extern int __VERIFIER_nondet_int();

/*main()
{
    int c, i;
    int textlen, patlen;
    int result;
    char text[tmax];
    char pattern[pmax];
    textlen = 0;
    i = 1;
    c=fgetc(stdin);
    while (c != '\n')
    {
        if (i <= tmax)
        {
            text[i] = c;
            printf("%c", c);
            i ++;
            c=fgetc(stdin);
            textlen = i -1;
        }
        else
        {
            i ++;
            c=fgetc(stdin);
        }
    };
    printf("\n");
    printf("textlen = %d \n", textlen);
    patlen = 0;
    i= 1;
    c=fgetc(stdin);
    while (c != '\n')
    {
        if (i <= pmax)
        {
            pattern[i] = c;
            printf("%c", c);
            i ++;
            c=fgetc(stdin);
            patlen = i - 1;
        }
        else
        {
            i ++;
            c=fgetc(stdin);
        }
    };
    printf("\n");
    printf("patlen = %d \n", patlen);
    result = stringmatch2(pattern, text, patlen, textlen);
    printf("%d\n", result);
}
*/

int stringmatch2(pattern, text, patlen, textlen)
char pattern[];
char text[];
int patlen, textlen;
{
    int patpos, textpos;
    patpos = 1;
    textpos = 1;
    if (textlen == 0)
        return(0);
    else
        ;
    if (patlen == 0)
        return(1);
    else
        ;
    while ( (patpos <= patlen) && (textpos <= textlen))
    {
        if (pattern[patpos] == text[textpos])
        {
            textpos = textpos + 1;
            patpos = patpos + 1;
        }
        else {
            textpos = (textpos - patpos) + 2;
            patpos = 1;
        }
    };
    if (patpos > patlen)
        return(textpos - patlen);
    else
        return(0);
}

void testme(){

   
    int c, i;
    int textlen, patlen;
    int result;
    textlen = 0;
    i = 1;
    c=__VERIFIER_nondet_int();
    while (c != '\n')
    {
        if (i <= tmax)
        {
            text[i] = c;
            printf("%c", c);
            i ++;
            c=__VERIFIER_nondet_int();
            textlen = i -1;
        }
        else
        {
            i ++;
            c=__VERIFIER_nondet_int();
        }
    };
    printf("\n");
    printf("textlen = %d \n", textlen);
    patlen = 0;
    i= 1;
    c=__VERIFIER_nondet_int();
    while (c != '\n')
    {
        if (i <= pmax)
        {
            pattern[i] = c;
            printf("%c", c);
            i ++;
            c=__VERIFIER_nondet_int();
            patlen = i - 1;
        }
        else
        {
            i ++;
            c=__VERIFIER_nondet_int();
        }
    };
    printf("\n");
    printf("patlen = %d \n", patlen);
    result = stringmatch2(pattern, text, patlen, textlen);
    printf("%d\n", result);
}
