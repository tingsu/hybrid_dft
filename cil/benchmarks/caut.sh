#!/bash/sh

# This file is to prepare the program under test for caut.
# It currently provides unit/branch/data flow testing(by caut or blast)
# Usage: source caut.sh $file_name $entry_fn/unit_fn $fuzz_command $fuzz_option

SRC_FILE_NAME=$1
SRC_PART_NAME=${1%.c}
SRC_FILE_DIR=${1%/*}
FILE_NAME=${SRC_PART_NAME##*/}
# entry function or unit name to test
ENTRY_FUNC=$2
# command 
FUZZ_COMMAND=$3
# command option
FUZZ_OPTION=$4


CIL_PATH=../cil-1.5.1/obj/x86_LINUX/


function reprocess_program(){
	echo "->caut init..."
	echo "remove old files..."
	rm -rf $SRC_FILE_NAME.db $SRC_PART_NAME.cil.orig.c $SRC_PART_NAME.cil.c
	
	echo "->preprocess file..."
	gcc -E -I ../ $SRC_FILE_NAME > $SRC_PART_NAME.i
}

function extract_cfg(){
	echo "->extract cfg info and insert cover function --> P"
	$CIL_PATH/cilly.byte.exe --noInsertImplicitCasts --useLogicalOperators --domakeCFG --docautcover -file $SRC_FILE_NAME -bc 1 --out $SRC_PART_NAME.cil.orig.c $SRC_PART_NAME.i
}

function simple_cil_pass(){

	echo "->invoke a pure CIL pass to simplify the program under test"
	$CIL_PATH/cilly.byte.exe --domakeCFG --out $SRC_PART_NAME.cil.simplified.c $SRC_PART_NAME.i

}

#echo "->create caut types function"
#Generate the function  "__CAUT_register_types"
#$CIL_PATH/cilly.byte.exe --out $SRC_PART_NAME.typ.c --docauttyps $SRC_PART_NAME.i

function simple_unit_testing(){

	reprocess_program
	extract_cfg
	
	echo "-> insert runtime interfaces --> P' (coverage-driven testing + unit testing level)"
	$CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME -cfunit $ENTRY_FUNC --unit_testing --out $SRC_PART_NAME.cil.c $SRC_PART_NAME.i > $SRC_PART_NAME.append
	
	echo "->append code..."
	cat $SRC_PART_NAME.append >> $SRC_PART_NAME.cil.c

	#comment "#line" line
	sed -i "s/\#line/\/\/\#line/g" $SRC_PART_NAME.cil.c
	sed -i "s/\#line/\/\/\#line/g" $SRC_PART_NAME.cil.orig.c

	echo "->add caut header file \"caut.h\""
	sed -i "1 i\\#include \"caut.h\"" $SRC_PART_NAME.cil.c
}

function unit_testing(){

	reprocess_program
	extract_cfg
	
	echo "-> insert runtime interfaces --> P' (coverage-driven testing + unit testing level + instrument inner function calls --> generate type system and print test case)"
	$CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME -cfunit $ENTRY_FUNC --unit_testing --instrument_funcation_call --generate_type_system --out $SRC_PART_NAME.cil.c $SRC_PART_NAME.i > $SRC_PART_NAME.append
	
	echo "->append code..."
	cat $SRC_PART_NAME.append >> $SRC_PART_NAME.cil.c

	#comment "#line" line
	sed -i "s/\#line/\/\/\#line/g" $SRC_PART_NAME.cil.c
	sed -i "s/\#line/\/\/\#line/g" $SRC_PART_NAME.cil.orig.c

	echo "->add caut header file \"caut.h\""
	sed -i "1 i\\#include \"caut.h\"" $SRC_PART_NAME.cil.c
}

function branch_testing(){

	reprocess_program
	extract_cfg
	
	echo "-> insert runtime interfaces --> P' (program testing level + branch testing)"
	echo "Remove Old Files..."
	rm -rf ${SRC_FILE_DIR}/obj-df-data
	echo "Call CIL to Analyze..."
	$CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --interp_br_testing --out $SRC_PART_NAME.cil.c $SRC_PART_NAME.i > $SRC_PART_NAME.append
	echo "Everything is Ready..."
	mkdir ${SRC_FILE_DIR}/obj-df-data
	mv ${SRC_FILE_DIR}/*.txt ${SRC_FILE_DIR}/obj-df-data
	
	echo "->append code..."
	cat $SRC_PART_NAME.append >> $SRC_PART_NAME.cil.c

	#comment "#line" line
	sed -i "s/\#line/\/\/\#line/g" $SRC_PART_NAME.cil.c
	sed -i "s/\#line/\/\/\#line/g" $SRC_PART_NAME.cil.orig.c

	echo "->add caut header file \"caut.h\""
	sed -i "1 i\\#include \"caut.h\"" $SRC_PART_NAME.cil.c
}

# transform the program to facilitate data flow testing by klee.
function transform_program_for_klee(){

	# preprocess the program by including header files (xx.c --> xx.i)
	reprocess_program

    echo "Remove Old Files..."
	rm -rf ${SRC_FILE_DIR}/obj-df-data

	echo "Call CIL to Analyze..."
	$CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --klee_instrumentation --transform_nondet_to_klee_make_symbolic --entry_fn $ENTRY_FUNC --out $SRC_PART_NAME.cil.c $SRC_PART_NAME.i
	
	echo "Everything is Ready..."
	mkdir ${SRC_FILE_DIR}/obj-df-data
	mv ${SRC_FILE_DIR}/*.txt ${SRC_FILE_DIR}/obj-df-data

	#comment "#line" line
	sed -i "s/\#line/\/\/\#line/g" $SRC_PART_NAME.cil.c

	#rm old dir
	rm -rf ${SRC_FILE_DIR}/obj-klee/
	mkdir ${SRC_FILE_DIR}/obj-klee/
	cp $SRC_PART_NAME.cil.c ${SRC_FILE_DIR}/obj-klee/

}	

# transform the program to count duas.
function transform_program_count_duas(){

    # preprocess the program by including header files (xx.c --> xx.i)
    reprocess_program
    echo "Call CIL to Analyze..."
    $CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --klee_instrumentation --transform_nondet_to_klee_make_symbolic --entry_fn $ENTRY_FUNC --out $SRC_PART_NAME.cil.c $SRC_PART_NAME.i

    echo "Everything is Ready..."
    #comment "#line" line
    sed -i "s/\#line/\/\/\#line/g" $SRC_PART_NAME.cil.c

}

function data_flow_testing(){
    reprocess_program
	extract_cfg
	
	echo "-> insert runtime interfaces --> P' (program testing level + on-demand data flow testing)"
	echo "Remove Old Files..."
	rm -rf ${SRC_FILE_DIR}/obj-df-data
	echo "Call CIL to Analyze..."
	$CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --interp_df_testing --entry_fn $ENTRY_FUNC --out $SRC_PART_NAME.cil.c $SRC_PART_NAME.i > $SRC_PART_NAME.append
	echo "Everything is Ready..."
	mkdir ${SRC_FILE_DIR}/obj-df-data
	mv ${SRC_FILE_DIR}/*.txt ${SRC_FILE_DIR}/obj-df-data

	echo "->append code..."
	cat $SRC_PART_NAME.append >> $SRC_PART_NAME.cil.c

	#comment "#line" line
	sed -i "s/\#line/\/\/\#line/g" $SRC_PART_NAME.cil.c
	sed -i "s/\#line/\/\/\#line/g" $SRC_PART_NAME.cil.orig.c

	echo "->add caut header file \"caut.h\""
	sed -i "1 i\\#include \"caut.h\"" $SRC_PART_NAME.cil.c
}

#transform the program to facilitate reachability checking by blast.
function transform_program_for_blast (){
	reprocess_program
	extract_cfg

    # count the duas, when 0 is given
    if [ $1 = "0" ]; then
        DUA_CNT=1
        echo "count duas for blast, and generate data files"
        #rm old dir
        rm -rf ${SRC_FILE_DIR}/obj-$2-data/
        mkdir -p ${SRC_FILE_DIR}/obj-$2-data/

        $CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --interp_df_testing --cegar_instrumentation --model_checker $2-data --entry_fn $ENTRY_FUNC --out ${SRC_FILE_DIR}/obj-$2-data/$FILE_NAME.cil.c $SRC_PART_NAME.i # 2>/dev/null

    else # instrument the duas one by one offline 
	
	    DUA_CNT=$1
	    echo "start transform program for blast"
	    echo "total dua cnt: $DUA_CNT"

	    #rm old dir
    	rm -rf ${SRC_FILE_DIR}/obj-$2/
    	mkdir ${SRC_FILE_DIR}/obj-$2/

        duas_data_file=${SRC_FILE_DIR}/obj-$2-data/$FILE_NAME.i.usedef.txt
        var_defs_data_file=${SRC_FILE_DIR}/obj-$2-data/$FILE_NAME.i.var_defs.txt

        if [[ ! -f "$duas_data_file" ]] || [[ ! -f "$var_defs_data_file" ]]; then
            echo "Under the offline instrumentation model, cannot find duas and var_defs data files!"
            exit 0
        fi
	
	    for((i=1; i<=$DUA_CNT; i++))
	    do
		    echo "process dua $i..."
		    $CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --interp_df_testing --cegar_instrumentation --model_checker $2 --offline_instrumentation --duas_data_file $duas_data_file --var_defs_data_file $var_defs_data_file --entry_fn $ENTRY_FUNC --cegar_dua_id $i --out ${SRC_FILE_DIR}/obj-$2/$FILE_NAME.cil_$i.c $SRC_PART_NAME.i  2>/dev/null
	    done
    fi
}

#transform the program to facilitate reachability checking by CPAchecker.
function transform_program_for_cpa (){
	reprocess_program
	extract_cfg

    # count the duas, when 0 is given
    if [ $1 = "0" ]; then
        DUA_CNT=1
        echo "count duas for cpa, and generate data files"
        #rm old dir
        rm -rf ${SRC_FILE_DIR}/obj-cpa-data/
        mkdir -p ${SRC_FILE_DIR}/obj-cpa-data/

        $CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --interp_df_testing --cegar_instrumentation --model_checker cpa-data --entry_fn $ENTRY_FUNC --out ${SRC_FILE_DIR}/obj-cpa-data/$FILE_NAME.cil.c $SRC_PART_NAME.i #2>/dev/null

    # instrument the duas one by one offline    
    else
	
	    DUA_CNT=$1
	    echo "start transform program for cpa"
	    echo "total dua cnt: $DUA_CNT"
	    #rm old dir
	    rm -rf ${SRC_FILE_DIR}/obj-cpa/
	    mkdir ${SRC_FILE_DIR}/obj-cpa/

        duas_data_file=${SRC_FILE_DIR}/obj-cpa-data/$FILE_NAME.i.usedef.txt
        var_defs_data_file=${SRC_FILE_DIR}/obj-cpa-data/$FILE_NAME.i.var_defs.txt

        if [[ ! -f "$duas_data_file" ]] || [[ ! -f "$var_defs_data_file" ]]; then
            echo "Under the offline instrumentation model, cannot find duas and var_defs data files!"
            exit 0
        fi
	
	    for((i=1; i<=$DUA_CNT; i++))
	    do
		    echo "process dua $i..."
		    $CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --interp_df_testing --cegar_instrumentation --model_checker cpa --offline_instrumentation --duas_data_file $duas_data_file --var_defs_data_file $var_defs_data_file --entry_fn $ENTRY_FUNC --cegar_dua_id $i --out ${SRC_FILE_DIR}/obj-cpa/$FILE_NAME.cil_$i.c $SRC_PART_NAME.i 2>/dev/null
	    done
    fi
}

#transform the program to facilitate reachability checking by CBMC.
function transform_program_for_cbmc (){
	reprocess_program
	extract_cfg

    # count the duas, when 0 is given
    if [ $1 = "0" ]; then
        echo "count duas for cbmc, and generate data files"
        #rm old dir
        rm -rf ${SRC_FILE_DIR}/obj-cbmc-data/
        mkdir -p ${SRC_FILE_DIR}/obj-cbmc-data/

        $CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --interp_df_testing --cbmc_instrumentation_one --model_checker cbmc-data --entry_fn $ENTRY_FUNC --out ${SRC_FILE_DIR}/obj-cbmc-data/$FILE_NAME.cil.c      $SRC_PART_NAME.i  #2>/dev/null

    #instrument the duas one by one offline 
    else
	
	    DUA_CNT=$1
	    echo "start transform program for cbmc"
	    echo "total dua cnt: $DUA_CNT"
    	#rm old dir
	    rm -rf ${SRC_FILE_DIR}/obj-cbmc/
	    mkdir ${SRC_FILE_DIR}/obj-cbmc/

        duas_data_file=${SRC_FILE_DIR}/obj-cbmc-data/$FILE_NAME.i.usedef.txt
        var_defs_data_file=${SRC_FILE_DIR}/obj-cbmc-data/$FILE_NAME.i.var_defs.txt

        if [[ ! -f "$duas_data_file" ]] || [[ ! -f "$var_defs_data_file" ]]; then
           echo "Under the offline instrumentation model, cannot find duas and var_defs data files!"
           exit 0
        fi
	
	    for((i=1; i<=$DUA_CNT; i++))
	    do
		    echo "process dua $i..."
		    $CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --interp_df_testing --cbmc_instrumentation_one --model_checker cbmc --offline_instrumentation --duas_data_file $duas_data_file --var_defs_data_file        $var_defs_data_file --entry_fn $ENTRY_FUNC --cbmc_dua_id $i --out ${SRC_FILE_DIR}/obj-cbmc/$FILE_NAME.cil_$i.c $SRC_PART_NAME.i #2>/dev/null
	    done
    fi
}

#transform the program to facilitate reachability checking by CBMC for all duas.
function transform_program_for_cbmc_all_duas (){

    reprocess_program
    extract_cfg

    DUA_CNT=$FUZZ_OPTION
    echo "start transform program for cbmc with all duas"
    echo "total dua cnt: $DUA_CNT"

    #rm old dir
    rm -rf ${SRC_FILE_DIR}/obj-cbmc-all/
    mkdir ${SRC_FILE_DIR}/obj-cbmc-all/
    
    echo "process all duas ..."
    $CIL_PATH/cilly.byte.exe --domakeCFG --no-split-structs --dotcg -cffile $SRC_FILE_NAME --interp_df_testing --cbmc_instrumentation_all --model_checker cbmc --entry_fn $ENTRY_FUNC --out ${SRC_FILE_DIR}/obj-cbmc-all/$FILE_NAME.cil.c $SRC_PART_NAME.i #2>/dev/null

}



# interp. data flow testing
if [ "$FUZZ_COMMAND" = "df_test" ]; then
	data_flow_testing
elif [ "$FUZZ_COMMAND" = "klee_df_test" ]; then
	transform_program_for_klee
elif [ "$FUZZ_COMMAND" = "count_duas" ]; then
    transform_program_count_duas 
# data flow testing by blast
elif [ "$FUZZ_COMMAND" = "blast_df_test" ]; then
	transform_program_for_blast $FUZZ_OPTION "blast"
elif [ "$FUZZ_COMMAND" = "blast_old_df_test" ]; then
    transform_program_for_blast $FUZZ_OPTION "blast-old"
# data flow testing by CPAchecker
elif [ "$FUZZ_COMMAND" = "cpa_df_test" ]; then
    transform_program_for_cpa $FUZZ_OPTION
elif [ "$FUZZ_COMMAND" = "cbmc_df_test" ]; then
    transform_program_for_cbmc $FUZZ_OPTION
elif [ "$FUZZ_COMMAND" = "cbmc_all_df_test" ]; then
    transform_program_for_cbmc_all_duas $FUZZ_OPTION
# interp. branch testing 
elif [ "$FUZZ_COMMAND" = "br_test" ]; then
	branch_testing
# intrap. unit testing
elif [ "$FUZZ_COMMAND" = "unit_test" ]; then
	unit_testing
# intrap. simple unit testing
elif [ "$FUZZ_COMMAND" = "simple_unit_test" ]; then
	simple_unit_testing
fi


