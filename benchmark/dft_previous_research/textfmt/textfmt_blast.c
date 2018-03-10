//The correct version of the text formatting program in Goodenough and Gerhart
//[11] was used with all seven reported faults fixed. Other routines in this program are
//based on the Pascal routines used by Frankl and Weiss [10]. TEXTFMT takes a text as input
//and formats it. The text can be viewed as a sequence of words separated by spaces, tabs,
//and new lines. The formatted output sequence of words is identical to the input sequence
//and satisfies the properties given by Gerhart et al. [11].

#include <stdio.h>
#define nl -1
#define et -2
#define bl -3
#define maxpos 10
typedef int btype[maxpos+1];
typedef enum boolean {false, true} garbage;
extern int bufpos;
extern int fill;
extern btype buffer;
extern enum boolean alarm;

//Added by Ting Su, convert stdin to an array
#define MAX 20
int input_stream[MAX];
int fd_index = 0;

//extern function for cbmc
extern int nondet_int();

textfmt_main()
{
    int bufpos;
    int k;
    int fill;
    int cw;
    btype buffer;
    enum boolean alarm;
    int incharacter();
    void outcharacter();
    alarm = false;
    fill = 0;
    bufpos = 0;

    do
    {
        cw = incharacter();
        if ((cw == bl) || (cw == nl) || (cw == et) )
            if (bufpos != 0)
            { 
                if ((fill + bufpos < maxpos) && (fill != 0))
                {
                    outcharacter(bl);
                    fill = fill + 1;
                }
                else
                {
                    outcharacter(nl);
                    fill = 0;
                }
                for (k=1; k <= bufpos; k++)
                    outcharacter(buffer[k]);
                fill = fill + bufpos;
                bufpos = 0;
            }
            else
                ;
            else
                if (bufpos == maxpos)
                {
                    alarm = true;
                    printf("oversized words");
                }
                else
                {
                    bufpos = bufpos + 1;
                    buffer[bufpos] = cw;
                }
    } while ( !(alarm || (cw == et)));
}

int incharacter()
{
    int ch;
    if (myeof())
    {
        return et;
    }
    else if (myeoln())
    {
        return nl;
    }
    else
    {
        ch = my_fgetc();
        if (ch == ' ' || ch == '\t')
            return bl;
        else
            return ch;
    }
}

void outcharacter(code)
int code;
{
    if (code == et)
    ;
    else if (code == nl)
        printf ("\n");
    else if (code == bl)
        printf(" ");
    else
        printf("%c", code);
}

int myeoln()
{
    int c;
    c = my_fgetc();
    if (c == '\n')
        return (1);
    else
    {
        my_ungetc();
        return (0);
    }
}
int myeof()
{
    int c;
    c = my_fgetc();
    if (c == -1)
        return (1);
    else
    {
        my_ungetc();
        return (0);
    }
}

//added by Ting Su.
int my_fgetc(){

   int c;
   if(fd_index>=0 && fd_index<MAX){
	c = input_stream[fd_index];
	fd_index ++;
	return c;
   }
   else
	return -1;
}

//added by Ting Su.
void my_ungetc(){

    fd_index --;
}


void testme(){

   int i;
   int c;
   for(i=0; i<MAX; i++){
	c = nondet_int();
	input_stream[i] = c;
   }
   textfmt_main();
}
