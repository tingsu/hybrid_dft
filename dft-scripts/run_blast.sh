#! /bin/sh

# This file is used to run transformed program by blast
# Usage: source run_blast.sh $command_option $file_name(tcas-obj-blast/tcas_v1_blast.c) $entry_fn $pcnt
# DO NOT USE ./tcas-obj-blast/tcas_v1_blast.c, blast will crash!!!

# Some note:
# Before using this script, you need to export the blast executable into PATH

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
# the version of blast, "new" or "old"
blast_version=$6

#use /usr/bin/time to record the wall-clock time of the exectution
time_command="/usr/bin/time -f "%e""

# source run_blast.sh run_blast_df_testing replace_v1-obj-blast/replace.c replace_entry 373
# source run_blast.sh run_blast_df_testing tcas_v1_caut-obj-blast/tcas_v1_caut.c alt_sep_test 124
# source run_blast.sh run_blast_df_testing space_test-obj-blast/MWControl.c WorkModeSTSAroundMoon 587
function run_blast (){

    blast_command=""
    blast_command_line=""

    if [ "$blast_version" = "old" ]
    then
        export PATH=$PATH:/home/lab301/blast/blast-2.5-linux-x86_64-bin
        echo $PATH
        
        # blast 2.5
        blast_command="timeout ${max_testing_time}s pblast.opt"

        # The following is some comments when running blast 2.5, it is a little different from blast 2.7.2
        # When i use "-cf", blast will crash!!!
        # we should use "-cfb"
        # In some cases, like space_test, blast run into OUT_OF_MEMORY only without "-cfb".
        # Another way we can try is to run blast on a x64 platform.
        # The command line for blast 2.5
        blast_command_line="-cfb -enable-recursion -cref -timeout $max_testing_time -quiet"

        echo "$ $blast_command $blast_command_line"

    else # "new"
        export PATH=$PATH:/home/lab301/blast/blast-2.7.3/bin
        echo $PATH

        # blast 2.7.3, add "timeout" due to blast's "-timeout" option sometimes lose effect
        blast_command="timeout ${max_testing_time}s ocamltune pblast.opt" #ocamltune is used by BLAST for large program verification

        # The following command line works on blast 2.7.3, the current latest version of BLAST
        # It is recommended by the maintainer of BLAST.
        blast_command_line="-enable-recursion -alias empty -cref -lattice -include-lattice symb -noprofile -nosserr -timeout $max_testing_time -quiet"
        # blast_command_line="-enable-recursion -alias empty -cref -lattice -noprofile -nosserr -timeout $max_testing_time -quiet"
        
        echo "$ $blast_command $blast_command_line"
    fi

	for((i=1; i<=$pcnt; i++))
	do
		echo "remove old files"
	        rm -rf $file_name_dir/$program_name.res_$i.txt
		echo "test: " $file_main_name.cil_$i.c
		echo ">>> test " $i " <<<" >> $file_name_dir/$program_name.res_$i.txt
		($time_command $blast_command $file_main_name.cil_$i.c $blast_command_line -main $entry_fn 1>/dev/null 2>>$file_name_dir/$program_name.res_$i.txt)
		#echo ">>> end  <<<" >> $file_main_name.res_$i.txt
	done
}

# concrete command line like these:
# source run_blast.sh analyze_blast_df_testing tcas_v1_caut-obj-blast/tcas_v1_caut.c alt_sep_test 124
function analyze_blast (){

    blast_result_file_name="" # the result file to record statistics from blast

    if [ "$blast_version" = "old" ]
    then
        blast_result_file_name=blast.2.5.df.$program_name.result
    else
        blast_result_file_name=blast.2.7.3.df.$program_name.result
    fi

	for((i=1; i<=$pcnt; i++))
	do
		# -n without newline
		echo -n "$i " >> $file_name_dir/$blast_result_file_name
		
		# get the execution time
		exec=`sed -n '$p' $file_name_dir/$program_name.res_$i.txt`
		echo -n "$exec " >> $file_name_dir/$blast_result_file_name
		
		# check safe/unsafe/error/timeout
		grep -w -o "unsafe" $file_name_dir/$program_name.res_$i.txt
		# $? get the reuturn value of GREP
		res=$?
		echo "res = $res"
		if [ "$res" = "0" ]; then
			echo "unsafe" >> $file_name_dir/$blast_result_file_name
		else
			grep -w -o "safe" $file_name_dir/$program_name.res_$i.txt
			res=$?
			echo "res = $res"
			if [ "$res" = "0" ]; then
				echo "safe" >> $file_name_dir/$blast_result_file_name
			else
				echo "error/timeout" >> $file_name_dir/$blast_result_file_name
			fi
		fi
		
	done
	
	# count unsafe/safe/error/timeout duas
	unsafe_dua_cnt=`grep -w -o "unsafe" --count $file_name_dir/$blast_result_file_name`
	safe_dua_cnt=`grep -w -o "safe" --count $file_name_dir/$blast_result_file_name`
	error_or_timeout_dua_cnt=`grep -w -o "timeout" --count $file_name_dir/$blast_result_file_name`
	
	echo "statistics: unsafe duas: $unsafe_dua_cnt, safe duas: $safe_dua_cnt, error/timeout duas: $error_or_timeout_dua_cnt"
    echo "data flow coverage: $(($unsafe_dua_cnt))/$(($pcnt-$safe_dua_cnt))"
    coverage_rate=`echo "scale = 2; $unsafe_dua_cnt * 100 / ($pcnt-$safe_dua_cnt)" | bc`
    echo "df_coverage:${coverage_rate}"
	
}


# run blast for data flow testing
if [ "$command_option" = "run_blast_df_testing" ]; then
	echo "run blast on data flow testing"
	echo $file_main_name
	echo "start running blast"
	run_blast
# analyze running statistics of blast
elif [ "$command_option" = "analyze_blast_df_testing" ]; then
	echo "analyze the result of blast"
	echo "remove old analyze results"
	rm -rf $file_name_dir/*.result
	analyze_blast
# error warning
else
	echo "you specify: $command_option"
	echo "wrong command options, \"run_blast_df_testing\", \"analyze_blast_df_testing\" "
fi


