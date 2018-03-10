#! /bin/sh

# This file is used to run transformed program by CPAchecker
# Usage: source run_cpa.sh $command_option $file_name(tcas-obj-blast/tcas_v1_blast.c) $entry_fn $pcnt

# search "safe" and give line number
# grep -w -n -o "safe"  ./tcas-obj-blast/result.txt
# count "unsafe", i.e., feasible pair
# grep -w -o "safe" --count ./tcas-obj-blast/result.txt


# command option
command_option=$1
#file name
file_name_under_test=$2
file_main_name=${2%.c}
program_name=${file_main_name##*/}
#the directory where the file under test locates
file_name_dir=${2%/*}
#the entry function
entry_fn=$3
#the total cnt of program variants
pcnt=$4
max_testing_time=$5

#use /usr/bin/time to record the wall-clock time of the exectution
time_command="/usr/bin/time -f "%e""
cpa_command="../CPAchecker/CPAchecker-1.6.1-unix/scripts/cpa.sh"
cpa_dir="/home/lab301/CPAchecker/CPAchecker-1.6.1-unix"

# The following command line works on CPAchecker 1.3.4
# It is recommended by the maintainer of CPAchecker.
cpa_command_line="-config ../CPAchecker/CPAchecker-1.6.1-unix/config/predicateAnalysis.properties -skipRecursion -timelimit $max_testing_time"


# the result file to record statistics from cpachecker
# file name for cpachecker
cpa_result_file_name=cpa.1.6.1.df.$program_name.result


# concrete command line like these:
# source run_cpa.sh run_cpa_df_testing tcas_v1_caut-obj-cpa/tcas_v1_cpa.c testme 124
function run_cpa (){

	for((i=1; i<=$pcnt; i++))
	do
		echo "remove old files"
	    rm -rf $file_name_dir/$program_name.res_$i.txt
		echo "test: " $file_main_name.cil_$i.c
		echo ">>> test " $i " <<<" >> $file_name_dir/$program_name.res_$i.txt
		($time_command $cpa_command $file_main_name.cil_$i.c $cpa_command_line -entryfunction $entry_fn 1>/dev/null 1>>$file_name_dir/$program_name.res_$i.txt 2>>$file_name_dir/$program_name.res_$i.txt)
		#echo ">>> end  <<<" >> $file_main_name.res_$i.txt
	done

    # remove the output files from cpachecker
    rm -rf output
}

# concrete command line like these:
# source run_cpa.sh analyze_cpa_df_testing tcas_v1_caut-obj-cpa/tcas_v1_cpa.c testme 124
function analyze_cpa (){

	for((i=1; i<=$pcnt; i++))
	do
		# -n without newline
		echo -n "$i " >> $file_name_dir/$cpa_result_file_name
		
		# get the execution time
		exec=`sed -n '$p' $file_name_dir/$program_name.res_$i.txt`
		echo -n "$exec " >> $file_name_dir/$cpa_result_file_name
		
		# check safe/unsafe/error/timeout
		grep -w -o "FALSE" $file_name_dir/$program_name.res_$i.txt
		# $? get the reuturn value of GREP
		res=$?
		echo "res = $res"
		if [ "$res" = "0" ]; then
			echo "unsafe" >> $file_name_dir/$cpa_result_file_name
		else
			grep -w -o "TRUE" $file_name_dir/$program_name.res_$i.txt
			res=$?
			echo "res = $res"
			if [ "$res" = "0" ]; then
				echo "safe" >> $file_name_dir/$cpa_result_file_name
			else 
                grep -w -o "UNKNOWN" $file_name_dir/$program_name.res_$i.txt
                res=$?
                echo "res = $res"
                if [ "$res" = "0" ]; then
                    echo "unknown" >> $file_name_dir/$cpa_result_file_name
                else
				    echo "error/timeout" >> $file_name_dir/$cpa_result_file_name
                fi
			fi
		fi
		
	done
	
	# count unsafe/safe/error/timeout duas
	unsafe_dua_cnt=`grep -w -o "unsafe" --count $file_name_dir/$cpa_result_file_name`
	safe_dua_cnt=`grep -w -o "safe" --count $file_name_dir/$cpa_result_file_name`
    unknown_dua_cnt=`grep -w -o "unknown" --count $file_name_dir/$cpa_result_file_name`
	error_or_timeout_dua_cnt=`grep -w -o "timeout" --count $file_name_dir/$cpa_result_file_name`
	
	echo "statistics: unsafe duas: $unsafe_dua_cnt, safe duas: $safe_dua_cnt, unknown duas: $unknown_dua_cnt, error/timeout duas: $error_or_timeout_dua_cnt"
    echo "data flow coverage: $(($unsafe_dua_cnt))/$(($pcnt-$safe_dua_cnt))"
    coverage_rate=`echo "scale = 2; $unsafe_dua_cnt * 100 / ($pcnt-$safe_dua_cnt)" | bc`
    echo "df_coverage:${coverage_rate}"
	
}


# run cpachecker for data flow testing
if [ "$command_option" = "run_cpa_df_testing" ]; then
	echo "run cpachecker on data flow testing"
	echo $file_main_name
	echo "start running cpachecker"
	run_cpa
# analyze running statistics of cpachecker
elif [ "$command_option" = "analyze_cpa_df_testing" ]; then
	echo "analyze the result of cpachecker"
	echo "remove old analyze results"
	rm -rf $file_name_dir/*.result
	analyze_cpa
# error warning
else
	echo "you specify: $command_option"
	echo "wrong command options, \"run_cpa_df_testing\", \"analyze_cpa_df_testing\" "
fi


