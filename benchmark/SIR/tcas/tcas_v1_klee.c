
/*  -*- Last-Edit:  Fri Jan 29 11:13:27 1993 by Tarak S. Goradia; -*- */
/* $Log: tcas.c,v $
 * Revision 1.2  1993/03/12  19:29:50  foster
 * Correct logic bug which didn't allow output of 2 - hf
 * */

#include <stdio.h>
#include <klee/klee.h>

#define OLEV       600		/* in feets/minute */
#define MAXALTDIFF 600		/* max altitude difference in feet */


#define MINSEP     300          /* min separation in feet */
#define NOZCROSS   100		/* in feet */
				/* variables */
typedef int bool;

int Cur_Vertical_Sep;
bool High_Confidence;
bool Two_of_Three_Reports_Valid;

int Own_Tracked_Alt;
int Own_Tracked_Alt_Rate;
int Other_Tracked_Alt;

int Alt_Layer_Value;		/* 0, 1, 2, 3 */
int Positive_RA_Alt_Thresh[4];

int Up_Separation;
int Down_Separation;

				/* state variables */
int Other_RAC;			/* NO_INTENT, DO_NOT_CLIMB, DO_NOT_DESCEND */
#define NO_INTENT 0
#define DO_NOT_CLIMB 1
#define DO_NOT_DESCEND 2

int Other_Capability;		/* TCAS_TA, OTHER */
#define TCAS_TA 1
#define OTHER 2

int Climb_Inhibit;		/* true/false */

#define UNRESOLVED 0
#define UPWARD_RA 1
#define DOWNWARD_RA 2


void initialize()
{
    Positive_RA_Alt_Thresh[0] = 400;
    Positive_RA_Alt_Thresh[1] = 500;
    Positive_RA_Alt_Thresh[2] = 640;
    Positive_RA_Alt_Thresh[3] = 740;
}

int ALIM ()
{
  //comment by me, random testing error!
  //if( Alt_Layer_Value >= 0 && Alt_Layer_Value <4)
	  return Positive_RA_Alt_Thresh[Alt_Layer_Value];
  //return Positive_RA_Alt_Thresh[0]; 
}

int Inhibit_Biased_Climb ()
{
    //return (Climb_Inhibit ? Up_Separation + NOZCROSS : Up_Separation);
    if(Climb_Inhibit == 0)
    	return Up_Separation;
    else
    	return Up_Separation + NOZCROSS;
}

bool Non_Crossing_Biased_Climb()
{
    int upward_preferred;
    int upward_crossing_situation;
    bool result;

    //upward_preferred = Inhibit_Biased_Climb() > Down_Separation;
    if(Inhibit_Biased_Climb() > Down_Separation){
    	upward_preferred = 1;
    }
    else{
    	upward_preferred = 0;
    }
    if (upward_preferred)
    {
		//result = !(Own_Below_Threat()) || ((Own_Below_Threat()) && (!(Down_Separation >= ALIM())));
		if( !(Own_Below_Threat()) || ((Own_Below_Threat()) && (!(Down_Separation >= ALIM()))) )
		{
			result = 1;
		}
		else{
			result = 0;
		}
    }
    else
    {	
		//result = Own_Above_Threat() && (Cur_Vertical_Sep >= MINSEP) && (Up_Separation >= ALIM());
		if( Own_Above_Threat() && (Cur_Vertical_Sep >= MINSEP) && (Up_Separation >= ALIM()) )
		{	result = 1;
	    } 
		else{
			result = 0;
		}
    }
    return result;
}

bool Non_Crossing_Biased_Descend()
{
    int upward_preferred;
    int upward_crossing_situation;
    bool result;

    //upward_preferred = Inhibit_Biased_Climb() > Down_Separation;
    if( Inhibit_Biased_Climb() > Down_Separation ){
    	upward_preferred = 1;
    }
    else{
    	upward_preferred = 0;
    }
    if (upward_preferred)
    {
	//result = Own_Below_Threat() && (Cur_Vertical_Sep >= MINSEP) && (Down_Separation >= ALIM());
	   if( Own_Below_Threat() && (Cur_Vertical_Sep >= MINSEP) && (Down_Separation >= ALIM()) ){
	   	result = 1;
	   }
	   else{
	   	result = 0;
	   }
    }
    else
    {
	    //result = !(Own_Above_Threat()) || ((Own_Above_Threat()) && (Up_Separation >= ALIM()));
		if( !(Own_Above_Threat()) || ((Own_Above_Threat()) && (Up_Separation >= ALIM())) ){
		 result =1;
	 }
		else{
		 result = 0;
	 }
    }
    
    return result;
}

bool Own_Below_Threat()
{
    //return (Own_Tracked_Alt < Other_Tracked_Alt);
    if( Own_Tracked_Alt < Other_Tracked_Alt )
    	return 1;
    else
    	return 0;
}

bool Own_Above_Threat()
{
    //return (Other_Tracked_Alt < Own_Tracked_Alt);
    if( Other_Tracked_Alt < Own_Tracked_Alt )
    	return 1;
    else
    	return 0;
}

int alt_sep_test() 
{
    bool enabled, tcas_equipped, intent_not_known;
    bool need_upward_RA, need_downward_RA;
    int alt_sep;

	initialize();
	
    //enabled = High_Confidence && (Own_Tracked_Alt_Rate <= OLEV) && (Cur_Vertical_Sep > MAXALTDIFF);
    if( High_Confidence && (Own_Tracked_Alt_Rate <= OLEV) && (Cur_Vertical_Sep > MAXALTDIFF) ){
    	enabled = 1;
    }
    else{
    	enabled = 0;
    }
    	
    //tcas_equipped = Other_Capability == TCAS_TA;
    if( Other_Capability == TCAS_TA ){
    	tcas_equipped = 1;
	}
    else{
    	tcas_equipped = 0;
    }	
    //intent_not_known = Two_of_Three_Reports_Valid && Other_RAC == NO_INTENT;
    if( Two_of_Three_Reports_Valid && Other_RAC == NO_INTENT ){
    	intent_not_known =1;
	}
    else{
    	intent_not_known = 0;
	}
    
    alt_sep = UNRESOLVED;
    
    
    if (enabled && ((tcas_equipped && intent_not_known) || !tcas_equipped))
    {
        
	//need_upward_RA = Non_Crossing_Biased_Climb() && Own_Below_Threat();
	if( Non_Crossing_Biased_Climb() && Own_Below_Threat() ){
	
	
		need_upward_RA = 1;
	}
	else{
		need_upward_RA = 0;
	}
	//need_downward_RA = Non_Crossing_Biased_Descend() && Own_Above_Threat();
	if( Non_Crossing_Biased_Descend() && Own_Above_Threat() ){
		need_downward_RA = 1;
	}
	else
	{
		need_downward_RA = 0;
	}
	if (need_upward_RA && need_downward_RA){
        /* unreachable: requires Own_Below_Threat and Own_Above_Threat
           to both be true - that requires Own_Tracked_Alt < Other_Tracked_Alt
           and Other_Tracked_Alt < Own_Tracked_Alt, which isn't possible */
           
		
	    alt_sep = UNRESOLVED;
	}
	else if (need_upward_RA){
	    
	    
	    alt_sep = UPWARD_RA;
	}
	else if (need_downward_RA){
	     
	    alt_sep = DOWNWARD_RA;
	}
	else{
	    alt_sep = UNRESOLVED;
	    }
    }
    
    
    return alt_sep;
}

/*
__tcas_main(argc, argv)
int argc;
char *argv[];
{
    if(argc < 13)
    {
	fprintf(stdout, "Error: Command line arguments are\n");
	fprintf(stdout, "Cur_Vertical_Sep, High_Confidence, Two_of_Three_Reports_Valid\n");
	fprintf(stdout, "Own_Tracked_Alt, Own_Tracked_Alt_Rate, Other_Tracked_Alt\n");
	fprintf(stdout, "Alt_Layer_Value, Up_Separation, Down_Separation\n");
	fprintf(stdout, "Other_RAC, Other_Capability, Climb_Inhibit\n");
	exit(1);
    }
    initialize();
    Cur_Vertical_Sep = atoi(argv[1]);
    High_Confidence = atoi(argv[2]);
    Two_of_Three_Reports_Valid = atoi(argv[3]);
    Own_Tracked_Alt = atoi(argv[4]);
    Own_Tracked_Alt_Rate = atoi(argv[5]);
    Other_Tracked_Alt = atoi(argv[6]);
    Alt_Layer_Value = atoi(argv[7]);
    Up_Separation = atoi(argv[8]);
    Down_Separation = atoi(argv[9]);
    Other_RAC = atoi(argv[10]);
    Other_Capability = atoi(argv[11]);
    Climb_Inhibit = atoi(argv[12]);

    fprintf(stdout, "%d\n", alt_sep_test());
    exit(0);
}
*/

int main(){

	klee_make_symbolic(&Cur_Vertical_Sep, sizeof(Cur_Vertical_Sep), "Cur_Vertical_Sep");
	klee_make_symbolic(&High_Confidence, sizeof(High_Confidence), "High_Confidence");
	klee_make_symbolic(&Two_of_Three_Reports_Valid, sizeof(Two_of_Three_Reports_Valid), "Two_of_Three_Reports_Valid");
	klee_make_symbolic(&Own_Tracked_Alt, sizeof(Own_Tracked_Alt), "Own_Tracked_Alt");
	klee_make_symbolic(&Own_Tracked_Alt_Rate, sizeof(Own_Tracked_Alt_Rate), "Own_Tracked_Alt_Rate");
	klee_make_symbolic(&Other_Tracked_Alt, sizeof(Other_Tracked_Alt), "Other_Tracked_Alt");
	klee_make_symbolic(&Alt_Layer_Value, sizeof(Alt_Layer_Value), "Alt_Layer_Value");
	klee_assume(Alt_Layer_Value>=0);
	klee_assume(Alt_Layer_Value<=3);
	klee_make_symbolic(&Up_Separation, sizeof(Up_Separation), "Up_Separation");
	klee_make_symbolic(&Down_Separation, sizeof(Down_Separation), "Down_Separation");
	klee_make_symbolic(&Other_RAC, sizeof(Other_RAC), "Other_RAC");
	klee_make_symbolic(&Other_Capability, sizeof(Other_Capability), "Other_Capability");
	klee_make_symbolic(&Climb_Inhibit, sizeof(Climb_Inhibit), "Climb_Inhibit");
	
	alt_sep_test();
}
