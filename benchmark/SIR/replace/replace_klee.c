/*  -*- Last-Edit:  Mon Dec  7 10:31:51 1992 by Tarak S. Goradia; -*- */

#include <stdlib.h>
extern void	exit();
# include <stdio.h>

char global_array[100];
int global_array_index=0;

void	Caseerror();

typedef char	bool;
# define false 0
# define true  1
# define REPLACE_NULL 0

//# define MAXSTR 100
# define MAXSTR 5  //use more smaller length but without affecting the program behaviors
# define MAXPAT MAXSTR

# define ENDSTR  '\0'
# define ESCAPE  '@'
# define CLOSURE '*'
# define BOL     '%'
# define EOL     '$'
# define ANY     '?'
# define CCL     '['
# define CCLEND  ']'
# define NEGATE  '^'
# define NCCL    '!'
# define LITCHAR 'c'
# define DITTO   -1
# define DASH    '-'

# define TAB     9
# define NEWLINE 10

# define CLOSIZE 1

//extern functions for CBMC
extern int nondet_int();
extern char nondet_char();

typedef char	character;
typedef char string[MAXSTR];


//rewrite fgets by Ting Su
char* my_fgets(char* s, int maxsize){

	int i;
	//for(i=0; i<maxsize; i++)
	//   s[i] = nondet_char(); //nondet_char() gives input string for replacement
	klee_make_symbolic(s, sizeof(char)*MAXSTR, "s"); //klee
	if(s[0] == '\0')
	   return REPLACE_NULL;
        else
	   return s;
}

bool
replace_getline(s, maxsize)
char	*s;
int	maxsize;
{
    char *result;
    //result = fgets(s, maxsize, stdin);
    result = my_fgets(s, maxsize);  //substitute fgets
    //return (result != REPLACE_NULL);
    if( result != REPLACE_NULL)
    	return 1;
    else
    	return 0;
}
int
addstr(c, outset, j, maxset)
char	c;
char	*outset;
int	*j;
int	maxset;
{
    bool	result;
    if (*j >= maxset)
	result = false;
    else {
	outset[*j] = c;
	*j = *j + 1;
	result = true;
    }
    return result;
}

char
esc(s, i)
char 	*s;
int	*i;
{
    char	result;
    if (s[*i] != ESCAPE)
	result = s[*i];
    else
	if (s[*i + 1] == ENDSTR)
	    result = ESCAPE;
	else 
	{
	    *i = *i + 1;
	    if (s[*i] == 'n')
		result = NEWLINE;
	    else
		if (s[*i] == 't')
		    result = TAB;
		else
		    result = s[*i];
	}
    return result;
}

void change();


//rewrite isalnum
int my_isalnum(char c){

	if(  (c>='0' && c<='9') ||
		 (c>='a' && c<='z') ||
		 (c>='A' && c<='Z') )
		return 1;
	else
		return 0;
}

 void
dodash(delim, src, i, dest, j, maxset)
char	delim;
char	*src;
int	*i;
char	*dest;
int	*j;
int	maxset;
{
    int	k;
    bool	junk;
    char	escjunk;

    while ((src[*i] != delim) && (src[*i] != ENDSTR)) 
    {
	if (src[*i] == ESCAPE) {
	    escjunk = esc(src, i);
	    junk = addstr(escjunk, dest, j, maxset);
	} else	
	    if (src[*i] != DASH)
		junk = addstr(src[*i], dest, j, maxset);
	    else if (*j <= 1 || src[*i + 1] == ENDSTR)
		junk = addstr(DASH, dest, j, maxset);
	    else if ((my_isalnum(src[*i - 1])) && (my_isalnum(src[*i + 1])) 
		&& (src[*i - 1] <= src[*i + 1]))
		{
		    for (k = src[*i-1]+1; k<=src[*i+1]; k++) 
		    {
			junk = addstr(k, dest, j, maxset);
		    }	
		    *i = *i + 1;	
		} 
	    else	
		junk = addstr(DASH, dest, j, maxset);
	(*i) = (*i) + 1;
    }
}

bool
getccl(arg, i, pat, j)
char	*arg;
int	*i;
char	*pat;
int	*j;
{
    int	jstart;
    bool	junk;

    *i = *i + 1;
    if (arg[*i] == NEGATE) {
	junk = addstr(NCCL, pat, j, MAXPAT);
	*i = *i + 1;
    } else
	junk = addstr(CCL, pat, j, MAXPAT);
    jstart = *j;
    junk = addstr(0, pat, j, MAXPAT);
    dodash(CCLEND, arg, i, pat, j, MAXPAT);
    pat[jstart] = *j - jstart - 1;
    //return (arg[*i] == CCLEND);
    if (arg[*i] == CCLEND)
    	return 1;
    else
    	return 0;
}

 void
stclose(pat, j, lastj)
char	*pat;
int	*j;
int	lastj;
{
    int	jt;
    int	jp;
    bool	junk;


    for (jp = *j - 1; jp >= lastj ; jp--) 
    {
	jt = jp + CLOSIZE;
	junk = addstr(pat[jp], pat, &jt, MAXPAT);
    }
    *j = *j + CLOSIZE;
    pat[lastj] = CLOSURE;
}

/*
bool in_set_2(c)
char c;
{
  return (c == BOL || c == EOL || c == CLOSURE);
}
*/      

//rewrite the function with explicit if statements by Ting Su
bool in_set_2(c)
char c;
{
  if( c == BOL || c == EOL || c == CLOSURE )
  	return 1;
  else
  	return 0;
}      


/*
bool in_pat_set(c)
char c;
{
  return (   c == LITCHAR || c == BOL  || c == EOL || c == ANY 
          || c == CCL     || c == NCCL || c == CLOSURE);
}
*/    

//rewrite the function with explicit if statements by Ting Su
bool in_pat_set(c)
char c;
{
  if( c == LITCHAR || c == BOL  || c == EOL || c == ANY 
          || c == CCL     || c == NCCL || c == CLOSURE)
          return 1;
  else
  	  return 0;
}  

int
makepat(arg, start, delim, pat)
char	*arg;
int	start;
char	delim;
char	*pat;
{
    int	result;
    int	i, j, lastj, lj;
    bool	done, junk;
    bool	getres;
    char	escjunk;

    j = 0;
    i = start;
    lastj = 0;
    done = false;
    while ((!done) && (arg[i] != delim) && (arg[i] != ENDSTR)) {
	lj = j;
	if ((arg[i] == ANY))
	    junk = addstr(ANY, pat, &j, MAXPAT);
	else if ((arg[i] == BOL) && (i == start))
	    junk = addstr(BOL, pat, &j, MAXPAT);
	else if ((arg[i] == EOL) && (arg[i+1] == delim))
	    junk = addstr(EOL, pat, &j, MAXPAT);
	else if ((arg[i] == CCL)) 
	{
	    getres = getccl(arg, &i, pat, &j);
	    done = (bool)(getres == false);
	} 
	else if ((arg[i] == CLOSURE) && (i > start)) 
	{
	    lj = lastj;
	    if (in_set_2(pat[lj]))
		done = true;
	    else
		stclose(pat, &j, lastj);
	} 
	else 
	{
	    junk = addstr(LITCHAR, pat, &j, MAXPAT);
	    escjunk = esc(arg, &i);
	    junk = addstr(escjunk, pat, &j, MAXPAT);
	}
	lastj = lj;
	if ((!done))
	    i = i + 1;
    }	
    junk = addstr(ENDSTR, pat, &j, MAXPAT);
    if ((done) || (arg[i] != delim))
	result = 0;
    else
	if ((!junk))
	    result = 0;
	else
	    result = i;
    return result;
}

int
getpat(arg, pat)
char*	arg;
char*	pat;
{
    int	makeres;

    makeres = makepat(arg, 0, ENDSTR, pat);
    //return (makeres > 0);
    if (makeres > 0)
    	return 1;
    else
    	return 0;
}

int
makesub(arg, from, delim, sub)
	char*	arg;
	int	from;
	character	delim;
	char*	sub;
{
    int  result;
    int	i, j;
    bool	junk;
    character	escjunk;

    j = 0;
    i = from;
    while ((arg[i] != delim) && (arg[i] != ENDSTR)) {
	if ((arg[i] == (unsigned)('&')))
	    junk = addstr(DITTO, sub, &j, MAXPAT);
	else {
	    escjunk = esc(arg, &i);
	    junk = addstr(escjunk, sub, &j, MAXPAT);
	}
	i = i + 1;
    }
    if (arg[i] != delim)
	result = 0;
    else {
	junk = addstr(ENDSTR, &(*sub), &j, MAXPAT);
	if ((!junk))
	    result = 0;
	else
	    result = i;
    }	
    return result;
}

bool
getsub(arg, sub)
	char*	arg;
	char*	sub;
{
    int	makeres;

    makeres = makesub(arg, 0, ENDSTR, sub);
    //return (makeres > 0);
    if  (makeres > 0)
    	return 1;
    else
    	return 0;
}

void subline();

 bool
locate(c, pat, offset)
	character	c;
	char *	pat;
	int	offset;
{
    int	i;
    bool flag;

    flag = false;
    i = offset + pat[offset];
    while ((i > offset))
    {
	if (c == pat[i]) {
	    flag = true;
	    i = offset;
	} else
	    i = i - 1;
    }
    return flag;
}

bool
omatch(lin, i, pat, j)
	char*	lin;
	int	*i;
	char*	pat;
	int	j;
{
    char	advance;
    bool result;
    
    advance = -1;
    if ((lin[*i] == ENDSTR))
	result = false;
    else 
    {
	if (!in_pat_set(pat[j]))
	{
	    (void)fprintf(stdout, "in omatch: can't happen\n");
	    abort();	
	} else
	{
	     switch (pat[j]) 
	     {			
	     case LITCHAR:
		 if (lin[*i] == pat[j + 1])
		     advance = 1;
		 break ;	
	     case BOL:
		 if (*i == 0)
		     advance = 0;
		 break ;
	     case ANY:
		 if (lin[*i] != NEWLINE)
		     advance = 1;
		 break ;
	     case EOL:
		 if (lin[*i] == NEWLINE)
		     advance = 0;
		 break ;
	     case CCL:
		 if (locate(lin[*i], pat, j + 1))
		     advance = 1;	
		 break ;
	     case NCCL:
		 if ((lin[*i] != NEWLINE) && (!locate(lin[*i], pat, j+1)))
		     advance = 1;	
		 break ;
	     default:
		 Caseerror(pat[j]);
	     };
	 }	
    }
    if ((advance >= 0)) 
    {
	*i = *i + advance;
	result = true;
    } else
	result = false;
    return result;
}


patsize(pat, n)
	char*	pat;
	int	n;
{
    int size;
    if (!in_pat_set(pat[n])) {
	(void)fprintf(stdout, "in patsize: can't happen\n");
	abort();
    } else
	switch (pat[n]) 
	{
	case LITCHAR: size = 2; break;
	    
	case BOL:  case EOL:  case ANY:
	    size = 1;
	    break;
	case CCL:  case NCCL:
	    size = pat[n + 1] + 2;
	    break ;
	case CLOSURE:
	    size = CLOSIZE;
	    break ;
	default:
	    Caseerror(pat[n]);
	}
    return size;
}

int
amatch(lin, offset, pat, j)
	char*	lin;
	int	offset;
	char*	pat;
	int	j;
{
    int	i, k;
    bool	result, done;

    done = false;
    while ((!done) && (pat[j] != ENDSTR))
	if ((pat[j] == CLOSURE)) {
	    j = j + patsize(pat, j);
	    i = offset;
	    while ((!done) && (lin[i] != ENDSTR)) {
		result = omatch(lin, &i, pat, j);
		if (!result)	
		    done = true;
	    }
	    done = false;
	    while ((!done) && (i >= offset)) {
		k = amatch(lin, i, pat, j + patsize(pat, j));
		if ((k >= 0))
		    done = true;
		else
		    i = i - 1;
	    }
	    offset = k;
	    done = true;
	} else {
	    result = omatch(lin, &offset, pat, j);
	    if ((!result)) {	
		offset = -1;
		done = true;
	    } else
		j = j + patsize(pat, j);
	}
     return offset;
}

void
putsub(lin, s1, s2, sub)
  char *	lin;
  int	s1, s2;
  char *	sub;
{
    int	i;
    int		j;

    i = 0;
    while ((sub[i] != ENDSTR)) {
	if ((sub[i] == DITTO))
	    for (j = s1; j < s2; j++) 
	    {
		fputc(lin[j],stdout);
		global_array[global_array_index]=lin[j];
		global_array_index++;
	    }	
	else	
	{
	    fputc(sub[i],stdout);
	    global_array[global_array_index]=sub[i];
		global_array_index++;
	}
	i = i + 1;
    }
}

void
subline(lin, pat, sub)
 char	*lin;
 char   *pat;
 char   *sub;
{	
	int	i, lastm, m;

	lastm = -1;
	i = 0;
	while ((lin[i] != ENDSTR)) 
	{
	    m = amatch(lin, i, pat, 0);
	    if ((m >= 0) && (lastm != m)) {
		putsub(lin, i, m, sub);
		lastm = m;
	    }
	    if ((m == -1) || (m == i)) {
		fputc(lin[i],stdout);
		global_array[global_array_index]=lin[i];
		global_array_index++;
		i = i + 1;
	    } else
		i = m;
	}
}

 void
change(pat, sub)
char *pat, *sub;
{
    string  line;
    bool result;
    
    result = replace_getline(line, MAXSTR);
    while ((result)) {
	subline(line, pat, sub);
	printf("%s\n", global_array);
	global_array_index = 0;	
	result = replace_getline(line, MAXSTR);
    }
}

//The program "replace" accepts two arguments, one pattern, one substitution, and then it waits there (enters into a loop), accepting an input string (implemented by function "replace_getline"), to do string replacement.
//inputs: argc, argv[1], argv[2], string_for_replacement
//usage: ./replace pat sub
int main() // "main" -> "test_main"
{

   char pat[MAXSTR];
   char sub[MAXSTR];
   bool result;

   int i;
   int argc;
   char argv_1[MAXSTR];
   char argv_2[MAXSTR];

   //argc = nondet_int();
   klee_make_symbolic(&argc, sizeof(argc), "argc"); //klee
   //for(i=0;i<MAXSTR; i++){
   //	argv_1[i] = nondet_char();
   //	argv_2[i] = nondet_char();
   //}
   klee_make_symbolic(argv_1, sizeof(char)*MAXSTR, "argv_1");
   klee_make_symbolic(argv_2, sizeof(char)*MAXSTR, "argv_2"); //klee
   

   if (argc < 2) 
   {   
       (void)fprintf(stdout, "usage: change from [to]\n");
       exit(1);
   };

   result = getpat(argv_1, pat);
   
   if (!result)
   {
       (void)fprintf(stdout, "change: illegal \"from\" pattern\n");
       exit(2);
   }

   if (argc >= 3)
   {
       result = getsub(argv_2, sub);
       if (!result)
       {	
	   (void)fprintf(stdout, "change: illegal \"to\" string\n");
	   exit(3);
       }
   } else
   {
       sub[0] = '\0';
   }
   
   change(pat, sub);
   return 0;
}

void
Caseerror(n)
	int	n;
{
	(void)fprintf(stdout, "Missing case limb: line %d\n", n);
	exit(4);
}
