//STRMAT1 and STRMAT2: Both programs [10] take as input text and a pattern of zero or
//more characters. If the pattern appears in the text then the position of the first occurrence
//of the pattern in the text is returned, otherwise a 0 is returned. Although both programs
//share the same specication, their structures are different. STRMAT1 has a do while loop
//whereas STRMAT2 accomplishes the same task with a while loop. This syntactic difference
//makes STRMAT1 and STRMAT2 behave as two distinct programs in terms of the number of
//feasible all-uses and non-equivalent mutants.

#include <stdio.h>
#include <klee/klee.h>
#define tmax 80
#define pmax 10
char text[80];  //input: text
char pattern[10];//input: pattern


//extern function for cbmc
extern int nondet_int();

/*
main()
{
    int c, i;
    int textlen, patlen;
    int result;
    char text[tmax];
    char pattern[pmax];
    i = 1;
    c=fgetc(stdin);
    while (c != '\n')
    { text[i] = c;
        i ++;
        c=fgetc(stdin);
    };
    textlen = i -1;
    i= 1;
    c=fgetc(stdin);
    while (c != '\n')
    { pattern[i] = c;
        i ++;
        c=fgetc(stdin);
    };
    patlen = i - 1;
    result = stringmatch1(pattern, text, patlen, textlen);
    printf("%d\n", result);
}
*/

int stringmatch1(pattern, text, patlen, textlen)
char pattern[];
char text[];
int patlen, textlen;
{
    int patpos, textpos;
    patpos = 1;
    textpos = 1;
    if (textlen > tmax)
        return (-1);
    else if (textlen == 0)
        return(0);
    else
        ;
    if (patlen > pmax)
        return (-2);
    else if (patlen == 0)
        return(1);
    else
        ;
    do
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
    } while ( (patpos <= patlen) && (textpos <= textlen));
    if (patpos > patlen)
        return(textpos - patlen);
    else
        return(0);
}

void testme(){
   int textlen, patlen;
   klee_make_symbolic(pattern, sizeof pattern, "pattern");
   klee_make_symbolic(text, sizeof text, "text");
   klee_make_symbolic(&textlen, sizeof(textlen), "textlen");
   klee_make_symbolic(&patlen, sizeof(patlen), "patlen");
   if((textlen > 0) && (textlen < tmax) && (patlen > 0 )&& (patlen < pmax))
       stringmatch1(pattern, text, patlen, textlen);
}
int main() {
    testme();
    return 0;
}
