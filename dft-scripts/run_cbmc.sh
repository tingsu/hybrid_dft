#! /bin/sh

# This file is used to run transformed program by CBMC
# Usage: source run_cbmc.sh $command_option $file_name(tcas-obj-cbmc/tcas_v1_cbmc.c) $entry_fn $pcnt

# search "safe" and give line number
# grep -w -n -o "safe"  ./tcas-obj-cbmc/result.txt
# count "unsafe", i.e., feasible pair
# grep -w -o "safe" --count ./tcas-obj-cbmc/result.txt


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
timeout=$5
max_unwind=$6
max_depth=$7

#use /usr/bin/time to record the wall-clock time of the exectution
time_command="timeout -s KILL ${timeout}s /usr/bin/time -f "%e""
cbmc_command="cbmc"


# The following command line works on CBMC 5.4+. (Note it is very important to tune appropraite values for "--unwind" and "--depth", otherwise, MiniSat will throw OutOfMemory exception)
#cbmc_command_line="--slice-formula -I . --unwind ${max_unwind}"
# with --depth
cbmc_command_line="--slice-formula -I . --unwind ${max_unwind} --depth ${max_depth}" 


# the result file to record statistics from cbmc
# file name for cbmc
cbmc_result_file_name=cbmc.df.$program_name.result


# concrete command line like these:
# source run_cbmc.sh run_cbmc_df_testing tcas_v1_caut-obj-cbmc/tcas_v1_caut.c alt_sep_test 124
function run_cbmc (){

    export PATH=$PATH:/home/lab301/cbmc-5.6
    echo $PATH

	for((i=1; i<=$pcnt; i++))
	do
		echo "remove old files"
     	rm -rf $file_name_dir/$program_name.res_$i.txt
		echo "test: " $file_main_name.cil_$i.c
		echo ">>> test " $i " <<<" >> $file_name_dir/$program_name.res_$i.txt
		($time_command $cbmc_command $file_main_name.cil_$i.c $cbmc_command_line -function $entry_fn 1>/dev/null 1>>$file_name_dir/$program_name.res_$i.txt 2>>$file_name_dir/$program_name.res_$i.txt)
		#echo ">>> end  <<<" >> $file_main_name.res_$i.txt
	done
}

function run_cbmc_for_all (){

    export PATH=$PATH:/home/lab301/cbmc-5.6
    echo $PATH

    echo "remove old files"
    rm -rf $file_name_dir/$program_name.res.txt
    echo "test: " $file_main_name.cil.c
    echo ">>> test " all pairs  " <<<" >> $file_name_dir/$program_name.res.txt
    ($time_command $cbmc_command $file_main_name.cil.c $cbmc_command_line -function $entry_fn 1>/dev/null 1>>$file_name_dir/$program_name.res.txt 2>>$file_name_dir/$program_name.res.txt)
    #echo ">>> end  <<<" >> $file_main_name.res.txt

}

# concrete command line like these:
# source run_cbmc.sh analyze_cbmc_df_testing tcas_v1_caut-obj-cbmc/tcas_v1_caut.c alt_sep_test 124
function analyze_cbmc (){

	for((i=1; i<=$pcnt; i++))
	do
		# -n without newline
		echo -n "$i " >> $file_name_dir/$cbmc_result_file_name
		
		# get the execution time
		exec=`sed -n '$p' $file_name_dir/$program_name.res_$i.txt`
		echo -n "$exec " >> $file_name_dir/$cbmc_result_file_name
		
		# check safe/unsafe/error/timeout
		grep -w -o "FAILED" $file_name_dir/$program_name.res_$i.txt
		# $? get the reuturn value of GREP
		res=$?
		echo "res = $res"
		if [ "$res" = "0" ]; then
			echo "unsafe" >> $file_name_dir/$cbmc_result_file_name
		else
			grep -w -o "SUCCESSFUL" $file_name_dir/$program_name.res_$i.txt
			res=$?
			echo "res = $res"
			if [ "$res" = "0" ]; then
				echo "safe" >> $file_name_dir/$cbmc_result_file_name
			else
				echo "error/timeout" >> $file_name_dir/$cbmc_result_file_name
			fi
		fi
		
	done
	
	# count unsafe/safe/error/timeout duas
	unsafe_dua_cnt=`grep -w -o "unsafe" --count $file_name_dir/$cbmc_result_file_name`
	safe_dua_cnt=`grep -w -o "safe" --count $file_name_dir/$cbmc_result_file_name`
	error_or_timeout_dua_cnt=`grep -w -o "timeout" --count $file_name_dir/$cbmc_result_file_name`
	
	echo "statistics: unsafe duas: $unsafe_dua_cnt, safe duas: $safe_dua_cnt, error/timeout duas: $error_or_timeout_dua_cnt"
	echo "data flow coverage: $(($unsafe_dua_cnt))/$(($pcnt-$safe_dua_cnt))"
    coverage_rate=`echo "scale = 2; $unsafe_dua_cnt * 100 / ($pcnt-$safe_dua_cnt)" | bc`
    echo "df_coverage:${coverage_rate}"
}

function analyze_cbmc_for_all(){

    echo "analyze cbmc results for all pairs"   
    result_file=$file_name_dir/$program_name.res.txt

    # get the execution time
    exec=`sed -n '$p' $file_name_dir/$program_name.res.txt`

    while read -r line
    do
        l="$line"
        if [[ $l == *"cbmc_df_flag"* ]]; then
            echo "line: $l"
            # get the dua id
            id=`echo $l | awk '{print $3}' | cut -d '_' -f 4`
            dua_id=$(( $id + 1 ))
            result=`echo $l | awk '{print $6}'`  
            dua_result=""
            if [ "$result" == "FAILURE" ]; then
                dua_result="unsafe"
            elif [ "$result" == "SUCCESS" ]; then
                dua_result="safe"
            else
                dua_result="error/timeout"
            fi
            
            echo -n $dua_id >> $file_name_dir/$cbmc_result_file_name
            echo -n " " >> $file_name_dir/$cbmc_result_file_name
            echo -n $exec >> $file_name_dir/$cbmc_result_file_name
            echo -n " " >> $file_name_dir/$cbmc_result_file_name
            echo $dua_result >> $file_name_dir/$cbmc_result_file_name 
        fi
    done < "$result_file"

    # count unsafe/safe/error/timeout duas
    unsafe_dua_cnt=`grep -w -o "unsafe" --count $file_name_dir/$cbmc_result_file_name`
    safe_dua_cnt=`grep -w -o "safe" --count $file_name_dir/$cbmc_result_file_name`
    error_or_timeout_dua_cnt=`grep -w -o "timeout" --count $file_name_dir/$cbmc_result_file_name`

    echo "statistics: unsafe duas: $unsafe_dua_cnt, safe duas: $safe_dua_cnt, error/timeout duas: $error_or_timeout_dua_cnt"
    echo "data flow coverage: $(($unsafe_dua_cnt))/$(($pcnt-$safe_dua_cnt))"
    coverage_rate=`echo "scale = 2; $unsafe_dua_cnt * 100 / ($pcnt-$safe_dua_cnt)" | bc`
    echo "df_coverage:${coverage_rate}"
}



# run cbmc for data flow testing
if [ "$command_option" = "run_cbmc_df_testing" ]; then
	echo "run cbmc on data flow testing"
	echo $file_main_name
	echo "start running cbmc"
	run_cbmc

elif [ "$command_option" = "run_cbmc_all_df_testing" ]; then
    echo "run cbmc on data flow testing for all pairs"
    echo $file_main_name
    echo "start running cbmc for all pairs"
    run_cbmc_for_all

# analyze running statistics of cbmc
elif [ "$command_option" = "analyze_cbmc_df_testing" ]; then
	echo "analyze the result of cbmc"
	echo "remove old analyze results"
	rm -rf $file_name_dir/*.result
	analyze_cbmc

elif [ "$command_option" = "analyze_cbmc_all_df_testing" ]; then
    echo "analyze the result of cbmc for all pairs"
    echo "remove old analyze results"
    rm -rf $file_name_dir/*.result
    analyze_cbmc_for_all
# error warning
else
	echo "you specify: $command_option"
	echo "Usage: source run_cbmc.sh run_cbmc_df_testing/analyze_cbmc_df_testing/run_cbmc_all_df_testing/analyze_cbmc_all_df_testing file_name entry_fn dua_cnt "
fi


