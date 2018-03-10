#include <klee/klee.h>
#include <stdio.h>
#define tmax 80
#define pmax 3
char text[tmax];  //input
char pattern[pmax]; //input

//extern function for cbmc
extern int nondet_int();

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
    patlen = 0;
    i= 1;
    c=fgetc(stdin);
    while (c != '\n')
    {
        if (i <= pmax)
        {
            pattern[i] = c;
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
    result = stringmatch2(pattern, text, patlen, textlen);
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
    int textlen, patlen;
    klee_make_symbolic(&textlen, sizeof(textlen), "textlen");
    klee_make_symbolic(&patlen, sizeof(patlen), "patlen");
    klee_make_symbolic(pattern, sizeof pattern, "pattern");
    klee_make_symbolic(text, sizeof text, "text");
    if((textlen > 0) && (textlen < tmax) && (patlen > 0) && (patlen < pmax))
        stringmatch2(pattern, text, patlen, textlen);
}
int main() {
    testme();
    return 0;
}
