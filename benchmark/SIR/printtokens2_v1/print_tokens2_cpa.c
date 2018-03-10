/***********************************************/
/*  assgnment.5  Shu Z. A00042813 for CS453    */
/*  using the tokenizer and stream module      */
/*  print_tokens.c Code                        */
/***********************************************/

/***********************************************/
/* NAME:	print_tokens                   */
/* INPUT:	a filename                     */
/* OUTPUT:      print out the token stream     */
/* DESCRIPTION: using the tokenizer interface  */
/*              to print out the token stream  */
/***********************************************/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "tokens.h"
#define TRUE 1
#define FALSE 0


//CBMC can support some built-in library functions in <string.h> but with overapproximation, e.g., strlen, strcpy

#define MAX_FILE_SIZE 8  //max file size
//char file_content[MAX_FILE_SIZE]={'a', ' ', '[', ' ', 'a', 'n', 'd', ' '}; //input content from a file, created by Ting Su
char file_content[MAX_FILE_SIZE]; //input content from a file, created by Ting Su
int file_fd_index=0; //file fd index

//extern function for cbmc
extern int __VERIFIER_nondet_int();
extern char __VERIFIER_nondet_char();

test_main(void) // "test_main"
{  char *fname;
   token tok;

   int argc;
   int argc = __VERIFIER_nondet_int();
   int i;
   for(i=0; i<MAX_FILE_SIZE; i++) //__VERIFIER_nondet_char
     file_content[i] = __VERIFIER_nondet_char();
    //token_stream tp;
     if(argc==1)                  /* if not given filename,take as '""' */
       {
        fname= (char *) malloc(sizeof(char));
        *fname = '\0'; 
       }
     else if(argc==2)
        //fname= argv[1];
	fname = '0';
     else
       { fprintf(stdout, "Error!,please give the token stream\n");
         //exit(0); //change all exit(0) to exit() due to CPAchecker cannot recognize exit(0)
         exit();
       }

    //tp=open_token_stream(fname);  /* open token stream */
    //tok=get_token(tp);

    tok = get_token(file_content); //read from the mocked file content

    while (is_eof_token(tok) ==FALSE) /* take one token each time until eof */
    {
       print_token(tok);
       tok=get_token(file_content);
    }
    print_token(tok); /* print eof signal */
    //exit(0);
    exit();
}

/* stream.c code */

/***********************************************/
/* NMAE:	open_character_stream          */
/* INPUT:       a filename                     */
/* OUTPUT:      a pointer to chacracter_stream */
/* DESCRIPTION: when not given a filename,     */
/*              open stdin,otherwise open      */
/*              the existed file               */
/***********************************************/
/*character_stream open_character_stream(fname)
char *fname;
{ character_stream fp;
  if(fname == NULL)
     fp=stdin;
  else if ((fp=fopen(fname,"r"))== NULL)
  {
       fprintf(stdout, "The file %s doesn't exists\n",fname);
       exit(0);
  }
  return(fp);
}
*/


//added by Ting Su.
char my_getc(fp)
char* fp;
{

   char c;
   if(file_fd_index>=0 && file_fd_index<MAX_FILE_SIZE){
	c = fp[file_fd_index];
	file_fd_index ++;
	return c;
   }
   else
	return EOF;
}

//added by Ting Su.
char my_ungetc(ch, fp)
char ch;
char* fp;
{
   char c;
   if(file_fd_index<=0)
   	c = EOF;
   else{
	file_fd_index--;
	fp[file_fd_index] = ch; //put it back
   }
   return c;
}

/**********************************************/
/* NAME:	get_char                      */
/* INPUT:       a pointer to char_stream      */
/* OUTPUT:      a character                   */
/**********************************************/
char get_char(fp)
char* fp;
{ char ch;
  ch=my_getc(fp);
  return(ch);
}

/***************************************************/
/* NAME:      unget_char                           */
/* INPUT:     a pointer to char_stream,a character */
/* OUTPUT:    a character                          */
/* DESCRIPTION:when unable to put back,return EOF  */
/***************************************************/
char unget_char(ch,fp)
char* fp;
char ch;
{ char c;
  c=my_ungetc(ch,fp);
  if(c ==EOF)
    {
     return(c);
    }
  else
     return(c);
}

/* tokenizer.c code */

//Ting Su: change the buffer size from 81 to 21 to improve the checking ability of CBMC
char buffer[11];  /* fixed array length MONI */ /* to store the token temporar */


static int is_spec_symbol();
static int is_token_end();
static unget_error();
static int is_keyword();
static int is_identifier();
static int is_num_constant();
static int is_char_constant();
static int is_str_constant();
static int is_comment();
static void print_spec_symbol();


//added by Ting Su
int my_isalpha(char ch){

	if( (ch>='A' && ch<='Z') || (ch>='a' && ch<='z'))
		return 1;
	else 
		return 0;
}

//added by Ting Su
int my_isdigit(char ch){

	if( ch>='0' && ch<='9')
		return 1;
	else
		return 0;
}


//added by Ting Su
/*int my_strlen(char* arr)
{
   int len =0;
   int i =0;
   while(arr[i]!='\0'){
	len ++;
	i++;
   }
   return len;
}*/

//added by Ting Su
/*int my_strcmp(char* arr1, char* arr2){

	int min_size,i;
	if( strlen(arr1) >= strlen(arr2))
		min_size = strlen(arr2);
	else
		min_size = strlen(arr1);
		
	for(i=0; i<min_size; i++){
	
		if( arr1[i] > arr2[i])
			return 1;
		else if( arr1[i] < arr2[i])
			return -1;
	}
	
	if( strlen(arr1) == strlen(arr2) )
		return 0;
	else if( strlen(arr1) > strlen(arr2))
		return 1;
	else
		return -1;
}
*/

/********************************************************/
/* NAME:	open_token_stream                       */
/* INPUT:       a filename                              */
/* OUTPUT:      a pointer to a token_stream             */
/* DESCRIPTION: when filename is EMPTY,choice standard  */
/*              input device as input source            */
/********************************************************/
/*token_stream open_token_stream(fname)
char *fname;
{
 token_stream fp;
 if(strcmp(fname,"")==0)
    fp=open_character_stream(NULL);
 else
    fp=open_character_stream(fname);
 return(fp);
}*/

/********************************************************/
/* NAME :	get_token                               */
/* INPUT: 	a pointer to the tokens_stream          */
/* OUTPUT:      a token                                 */
/* DESCRIPTION: according the syntax of tokens,dealing  */
/*              with different case  and get one token  */
/********************************************************/
token get_token(tp)
//token_stream tp;
char* tp;
{ 
  int i=0,j;
  int id=0;
  char ch,ch1[2];
  for (j=0;j<=10;j++)          /* initial the buffer   */
      { buffer[j]='\0';} 
   ch1[0]='\0';
   ch1[1]='\0';
   ch=get_char(tp);
   while(ch==' '||ch=='\n')      /* strip all blanks until meet characters */
      {
       ch=get_char(tp);
      } 
   buffer[i]=ch;
   if(is_eof_token(buffer)==TRUE)return(buffer);
   if(is_spec_symbol(buffer)==TRUE) return(buffer); 
   if(ch =='"')id=1;    /* prepare for string */
   if(ch ==59)id=2;    /* prepare for comment */
   ch=get_char(tp);

   while (is_token_end(id,ch) == FALSE)/* until meet the end character */
   {
       i++;
       buffer[i]=ch;
       ch=get_char(tp);
   }
   ch1[0]=ch;                        /* hold the end charcater          */
   if(is_eof_token(ch1)==TRUE)       /* if end character is eof token    */
      { ch=unget_char(ch,tp);        /* then put back eof on token_stream */
        if(ch==EOF)unget_error(tp);
        return(buffer);
      }
   if(is_spec_symbol(ch1)==TRUE)     /* if end character is special_symbol */
      { ch=unget_char(ch,tp);        /* then put back this character       */
        if(ch==EOF)unget_error(tp);
        return(buffer);
      }
   if(id==1)                  /* if end character is " and is string */
     { i++;                     /* case,hold the second " in buffer    */
       buffer[i]=ch;
       return(buffer); 
     }
  return(buffer);                   /* return nomal case token             */
}

/*******************************************************/
/* NAME:	is_token_end                           */
/* INPUT:       a character,a token status             */
/* OUTPUT:	a BOOLEAN value                        */
/*******************************************************/
static int is_token_end(str_com_id,ch)
char ch;
int str_com_id;
{ char ch1[2];  /* fixed array declaration MONI */
 ch1[0]=ch;
 ch1[1]='\0';
 if(is_eof_token(ch1)==TRUE)return(TRUE); /* is eof token? */
 if(str_com_id==1)          /* is string token */
    { if(ch=='"' | ch=='\n')   /* for string until meet another " */
         return(TRUE);
      else
         return(FALSE);
    }

 if(str_com_id==2)    /* is comment token */
   { if(ch=='\n')     /* for comment until meet end of line */
        return(TRUE);
      else
        return(FALSE);
   }

 if(is_spec_symbol(ch1)==TRUE) return(TRUE); /* is special_symbol? */
 if(ch ==' ' || ch=='\n' || ch==59) return(TRUE); 
                              /* others until meet blank or tab or 59 */
 return(FALSE);               /* other case,return FALSE */
}

/****************************************************/
/* NAME :	token_type                          */
/* INPUT:       a pointer to the token              */
/* OUTPUT:      an integer value                    */
/* DESCRIPTION: the integer value is corresponding  */
/*              to the different token type         */
/****************************************************/
static int token_type(tok)
token tok;
{ 
 if(is_keyword(tok))return(keyword);
 if(is_spec_symbol(tok))return(spec_symbol);
 if(is_identifier(tok))return(identifier);
 if(is_num_constant(tok))return(num_constant);
 if(is_str_constant(tok))return(str_constant);
 if(is_char_constant(tok))return(char_constant);
 if(is_comment(tok))return(comment);
 if(is_eof_token(tok))return(end);
 return(error);                    /* else look as error token */
}

/****************************************************/
/* NAME:	print_token                         */
/* INPUT:	a pointer to the token              */
/* OUTPUT:      a BOOLEAN value,print out the token */
/*              according the forms required        */
/****************************************************/
int print_token(tok)
token tok;
{ int type;
  type=token_type(tok);
 if(type==error)
   { fprintf(stdout, "error,\"%s\".\n",tok);
   } 
 if(type==keyword)
   {fprintf(stdout, "keyword,\"%s\".\n",tok);
   }
 if(type==spec_symbol)print_spec_symbol(tok);
 if(type==identifier)
   {fprintf(stdout, "identifier,\"%s\".\n",tok);
   }
 if(type==num_constant)
   {fprintf(stdout, "numeric,%s.\n",tok);
   }
 if(type==str_constant)
   {fprintf(stdout, "string,%s.\n",tok);
   }
 if(type==char_constant)
   {tok=tok+1;
    fprintf(stdout, "character,\"%s\".\n",tok);
   }
 if(type==end) 
   fprintf(stdout, "eof.\n");
   }

/* the code for tokens judgment function */

/*************************************/
/* NAME:	is_eof_token         */
/* INPUT: 	a pointer to a token */
/* OUTPUT:      a BOOLEAN value      */
/*************************************/
int is_eof_token(tok)
token tok;
{ 
  if( *tok==EOF)
      return(TRUE);
  else
      return(FALSE);
}

/*************************************/
/* NAME:	is_comment           */
/* INPUT: 	a pointer to a token */
/* OUTPUT:      a BOOLEAN value      */
/*************************************/
static int is_comment(ident)
token ident;
{
  if( (*ident) ==59 )   /* the char is 59   */
     return(TRUE);
  else
     return(FALSE);
}

/*************************************/
/* NAME:	is_keyword           */
/* INPUT: 	a pointer to a token */
/* OUTPUT:      a BOOLEAN value      */
/*************************************/
static int is_keyword(str)
    token  str;
{ 
 if (!strcmp(str,"and") || !strcmp(str,"or") || !strcmp(str,"if") ||
    !strcmp(str,"xor")||!strcmp(str,"lambda")||!strcmp(str,"=>"))
      return(TRUE);
  else 
      return(FALSE);
}

/*************************************/
/* NAME:	is_char_constant     */
/* INPUT: 	a pointer to a token */
/* OUTPUT:      a BOOLEAN value      */
/*************************************/
static int is_char_constant(str)
    token str;
{
  if ((*str)=='#' && my_isalpha(*(str+1)))
     return(TRUE);
  else  
     return(FALSE);
}

/*************************************/
/* NAME:	is_num_constant      */
/* INPUT: 	a pointer to a token */
/* OUTPUT:      a BOOLEAN value      */
/*************************************/
static int is_num_constant(str)
    token  str;
{
  int i=1;
  
  if ( my_isdigit(*str)) 
    {
    while ( *(str+i) != '\0' )   /* until meet token end sign */
      {
       if(my_isdigit(*(str+i)))
         i++;
       else
         return(FALSE);
      }                         /* end WHILE */
    return(TRUE);
    }
  else
   return(FALSE);               /* other return FALSE */
}

/*************************************/
/* NAME:	is_str_constant      */
/* INPUT: 	a pointer to a token */
/* OUTPUT:      a BOOLEAN value      */
/*************************************/
static int is_str_constant(str)
    token str;
{
  int i=1;
 
  if ( *str =='"')
     { while (*(str+i)!='\0')  /* until meet the token end sign */
         { if(*(str+i)=='"')
             return(TRUE);        /* meet the second '"'           */
           else
           i++;
         }               /* end WHILE */
     return(FALSE);
    }
  else
    return(FALSE);       /* other return FALSE */
}



/*************************************/
/* NAME:	is_identifier         */
/* INPUT: 	a pointer to a token */
/* OUTPUT:      a BOOLEAN value      */
/*************************************/
static int is_identifier(str)
    token  str;
{
  int i=1;

  if ( my_isalpha( *str) ) 
     {
        while(  *(str+i) !='\0' )   /* unti meet the end token sign */
           { 
            if(my_isalpha(*(str+i)) || my_isdigit(*(str+i)))   
               i++;
            else
               return(FALSE);
           }      /* end WHILE */
     return(TRUE);
     }
  else
     return(FALSE);
}

/******************************************/
/* NAME:	unget_error               */
/* INPUT:       a pointer to token stream */
/* OUTPUT: 	print error message       */
/******************************************/
static unget_error(fp)
char *fp;
{
fprintf(stdout,"It can not get charcter\n");
}




/*************************************************/
/* NAME:        print_spec_symbol                */
/* INPUT:       a pointer to a spec_symbol token */
/* OUTPUT :     print out the spec_symbol token  */
/*              according to the form required   */
/*************************************************/
static void print_spec_symbol(str)
token str;
{
    if      (!strcmp(str,"("))
    {
             fprintf(stdout, "%s\n","lparen.");
             return;
    } 
    if (!strcmp(str,")"))
    {
             fprintf(stdout, "%s\n","rparen.");
             return;
    }
    if (!strcmp(str,"["))
    {
             fprintf(stdout, "%s\n","lsquare.");
             return;
    }
    if (!strcmp(str,"]"))
    {
             fprintf(stdout, "%s\n","rsquare.");
             return;
    }
    if (!strcmp(str,"'"))
    {
             fprintf(stdout, "%s\n","quote.");
             return;
    }
    if (!strcmp(str,"`"))
    {
             fprintf(stdout, "%s\n","bquote.");
             return;
    }
    
             fprintf(stdout, "%s\n","comma.");
}


/*************************************/
/* NAME:        is_spec_symbol       */
/* INPUT:       a pointer to a token */
/* OUTPUT:      a BOOLEAN value      */
/*************************************/
static int is_spec_symbol(str)
    token str;
{
    if (!strcmp(str,"("))
    {  
        return(TRUE);
    }
    if (!strcmp(str,")"))
    {
        return(TRUE);
    }
    if (!strcmp(str,"["))
    {
        return(TRUE);
    }
    if (!strcmp(str,"]"))
    {
        return(TRUE);
    }
    if (!strcmp(str,"'"))
    {
        return(TRUE);
    }
    if (!strcmp(str,"`"))
    {
        return(TRUE);
    }
    if (!strcmp(str,","))
    {
        return(TRUE);
    }
    return(FALSE);     /* others return FALSE */
}


