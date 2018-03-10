
/* $Log: schedule.c,v $
 * Revision 1.4  1993/05/04  12:23:58  foster
 * Debug stuff removed
 *
 * Revision 1.3  1993/05/03  20:27:04  foster
 * Full Functionality
 *
 * Revision 1.2  1993/05/03  15:41:01  foster
 * Restructure functions
 *
 * Revision 1.1  1993/05/01  11:38:04  foster
 * Initial revision
 * */

#include <stdio.h>
#include <stdlib.h>
#include "schedule2.h"

//extern functions for CBMC
extern int nondet_int();
extern double nondet_double();

static struct process * current_job;
static int next_pid = 0;

int
enqueue(prio, new_process)
     int prio;
     struct process *new_process;
{
    int status;
    if(status = put_end(prio, new_process)) return(status); /* Error */
    return(reschedule(prio));
}

struct queue
{
    int length;
    struct process *head;
};

static struct queue prio_queue[MAXPRIO + 1]; /* blocked queue is [0] */


//input: argc, argv[1], argv[2], argv[3]
//change float type to double type for all variable, otherwise, it always dump "type mismatch with get_process"
test_main(void) /* n3, n2, n1 : # of processes at prio3 ... */  //"test_main"
{
    int command, prio;
    double ratio;
    int nprocs, status, pid;
    struct process *process;

    // assign inputs by Ting Su
    int argc = nondet_int();
    int argv[4] ; // init. argv[4]
    argv[1] = nondet_int();
    argv[2] = nondet_int();
    argv[3] = nondet_int();

    

    if(argc != MAXPRIO + 1) exit_here(BADNOARGS);
    for(prio = MAXPRIO; prio > 0; prio--)
    {
	if((nprocs = argv[MAXPRIO + 1 - prio]) < 0) exit_here(BADARG);
	for(; nprocs > 0; nprocs--)
	{
	    if(status = new_job(prio)) exit_here(status);
	}
    }
    /* while there are commands, schedule it */
    while((status = get_command(&command, &prio, &ratio)) > 0)
    {
	schedule(command, prio, ratio);
    }
    if(status < 0) exit_here(status); /* Real bad error */
    exit_here(OK);
}


//substitution for fscanf
int my_fscanf_int(int* command){

   int* v = NULL;
   int ret = nondet_int();
   if(ret == -1)  // if -1, we assume Ctrl+D, the end of the input
	return EOF;
   else{
      v = (int*)malloc(sizeof(int));
      *v = ret;
      *command = *v;
     return 1;   // read successfully
   }
}

//substitution for fscanf
/*
int my_fscanf_double(double* ratio){

   double *v = NULL;
   double ret = nondet_double();
   if(ret == -1.0)  // if -1, we assume Ctrl+D, the end of the input
	return EOF;
   else{
      v = (double*)malloc(sizeof(double));
      *v = ret;
      *ratio = *v;
     return 1;   // read successfully
   }
}
*/

int 
get_command(command, prio, ratio)
    int *command, *prio;
    double *ratio;
{
    int status = OK;
    //char buf[CMDSIZE];
  
    int input_int;   //the input value of type "int"
    //if(fgets(buf, CMDSIZE, stdin))
    if(my_fscanf_int(&input_int))
    {
	*prio = *command = -1; *ratio =-1.0;
	//sscanf(buf, "%d", command);
	*command = input_int;
	switch(*command) //Ting Su: only these three commands "NEW_JOB", "UNBLOCK", "UPGRADE_PRIO" accept additional arguments
	{
	  case NEW_JOB :
	    //sscanf(buf, "%*s%d", prio);
	    *prio = nondet_int(); 
	    break;
	  case UNBLOCK :
	    //sscanf(buf, "%*s%f", ratio);
            *ratio = nondet_double();
	    break;
	  case UPGRADE_PRIO :
	    //sscanf(buf, "%*s%d%f", prio, ratio);
            *prio = nondet_int(); 
	    *ratio = nondet_double();
	    break;
	}
	 /* Find end of  line of input if no EOF */
	//while(buf[strlen(buf)-1] != '\n' && fgets(buf, CMDSIZE, stdin));
	return(TRUE);
    }
    else return(FALSE);
}

exit_here(status)
     int status;
{
    exit(abs(status));
}


int 
new_job(prio) /* allocate new pid and process block. Stick at end */
     int prio;
{
    int pid, status = OK;
    struct process *new_process;
    pid = next_pid++;
    new_process = (struct process *) malloc(sizeof(struct process));
    if(!new_process) status = MALLOC_ERR;
    else
    {
	new_process->pid = pid;
	new_process->priority = prio;
	new_process->next = (struct process *) 0;
	status = enqueue(prio, new_process);
	if(status)
	{assert(0);
	    free(new_process); /* Return process block */
	}
    }
    if(status) next_pid--; /* Unsuccess. Restore pid */
    return(status);
}

int upgrade_prio(prio, ratio) /* increment priority at ratio in queue */
     int prio;
     double ratio;
{
    int status;
    struct process * job;
/*    if(prio < 1 || prio > MAXLOPRIO) return(BADPRIO); MISSING CODE */
    if((status = get_process(prio, ratio, &job)) <= 0) return(status);
    /* We found a job in that queue. Upgrade it */
    job->priority = prio + 1;
    return(enqueue(prio + 1, job));
}

int
block() /* Put current job in blocked queue */
{
    struct process * job;
    job = get_current();
    if(job)
    {
	current_job = (struct process *)0; /* remove it */
	return(enqueue(BLOCKPRIO, job)); /* put into blocked queue */
    }
    return(OK);
}

int
unblock(ratio) /* Restore job @ ratio in blocked queue to its queue */
     double ratio;
{
    int status;
    struct process * job;
    if((status = get_process(BLOCKPRIO, ratio, &job)) <= 0) return(status);
    /* We found a blocked process. Put it where it belongs. */
    return(enqueue(job->priority, job));
}

int
quantum_expire() /* put current job at end of its queue */
{
    struct process * job;
    job = get_current();
    if(job)
    {
	current_job = (struct process *)0; /* remove it */
	return(enqueue(job->priority, job));
    }
    return(OK);
}

int
finish() /* Get current job, print it, and zap it. */
{
    struct process * job;
    job = get_current();
    if(job)
    {
	current_job = (struct process *)0;
	reschedule(0);
	fprintf(stdout, " %d", job->pid);
	free(job);
	return(FALSE);
    }
    else return(TRUE);
}

int
flush() /* Get all jobs in priority queues & zap them */
{
    while(!finish());
    fprintf(stdout, "\n");
    return(OK);
}

struct process * 
get_current() /* If no current process, get it. Return it */
{
    int prio;
    if(!current_job)
    {
	for(prio = MAXPRIO; prio > 0; prio--)
	{ /* find head of highest queue with a process */
	    if(get_process(prio, 0.0, &current_job) > 0) break;
	}
    }
    return(current_job);
}

int
reschedule(prio) /* Put highest priority job into current_job */
     int prio;
{
    if(current_job && prio > current_job->priority)
    {
	put_end(current_job->priority, current_job);
	current_job = (struct process *)0;
    }
    get_current(); /* Reschedule */
    return(OK);
}

int 
schedule(command, prio, ratio)
    int command, prio;
    double ratio;
{
    int status = OK;
    switch(command)
    {
      case NEW_JOB :
        status = new_job(prio);
	break;
      case QUANTUM_EXPIRE :
        status = quantum_expire();
	break;
      case UPGRADE_PRIO :
        status = upgrade_prio(prio, ratio);
	break;
      case BLOCK :
        status = block();
	break;
      case UNBLOCK :
        status = unblock(ratio);
	break;
      case FINISH :
        finish();
	fprintf(stdout, "\n");
	break;
      case FLUSH :
        status = flush();
	break;
      default:
	status = NO_COMMAND;
    }
    return(status);
}




int 
put_end(prio, process) /* Put process at end of queue */
     int prio;
     struct process *process;
{
    struct process **next;
    if(prio > MAXPRIO || prio < 0) return(BADPRIO); /* Somebody goofed */
     /* find end of queue */
    for(next = &prio_queue[prio].head; *next; next = &(*next)->next);
    *next = process;
    prio_queue[prio].length++;
    return(OK);
}

int 
get_process(prio, ratio, job)
     int prio;
     double ratio;
     struct process ** job;
{
    int length, index;
    struct process **next;
    if(prio > MAXPRIO || prio < 0) return(BADPRIO); /* Somebody goofed */
    if(ratio < 0.0 || ratio > 1.0) return(BADRATIO); /* Somebody else goofed */
    length = prio_queue[prio].length;
    index = ratio * length;
    index = index >= length ? length -1 : index; /* If ratio == 1.0 */
    for(next = &prio_queue[prio].head; index && *next; index--)
        next = &(*next)->next; /* Count up to it */
    *job = *next;
    if(*job)
    {
	*next = (*next)->next; /* Mend the chain */
	(*job)->next = (struct process *) 0; /* break this link */
	prio_queue[prio].length--;
	return(TRUE);
    }
    else return(FALSE);
}
