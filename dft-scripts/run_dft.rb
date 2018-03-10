#!/usr/bin/env ruby

# This script runs model checkers (including cbmc, blast, cpachecker) for data flow testing on all 35 benchmark subjects.
#
# 1. subjects from previous research work, totally including 7 subjects (power, triangle, find, factorization, stringmatch1, stringmatch2, textfmt). 
#

$subjects_loc=["dft_previous_research_benchmark/factorization", # benchmark from previous dft research work
           "dft_previous_research_benchmark/find",
           "dft_previous_research_benchmark/power",
           "dft_previous_research_benchmark/strmat1",
           "dft_previous_research_benchmark/strmat2",
           "dft_previous_research_benchmark/textfmt",
           "dft_previous_research_benchmark/triangle",

           "tcas-src",      # benchmark from SIR
           "replace_v1",
           "totinfo",
           "printtokens_v1",
           "printtokens2_v1",
           "schedule",
           "schedule2",

           "ssh-simplified/s3_clnt_1",  # benchmark from software verification competition
           "ssh-simplified/s3_clnt_3_termination",
           "ssh-simplified/s3_srvr_1",
           "ssh-simplified/s3_srvr_2",
           "ssh-simplified/s3_srvr_6",
           "ssh-simplified/s3_srvr_7",
           "ssh-simplified/s3_srvr_8",
           "ssh-simplified/s3_srvr_10",
           "ssh-simplified/s3_srvr_11",
           "ssh-simplified/s3_srvr_12",
           "ssh-simplified/s3_srvr_13",
           "ssh-simplified/s3_srvr_14",
           "ssh-simplified/s3_srvr_1a",
           "ssh-simplified/s3_srvr_1b",

           "ntdrivers-simplified/cdaudio_simpl1",
           "ntdrivers-simplified/diskperf_simpl1",
           "ntdrivers-simplified/floppy_simpl3",
           "ntdrivers-simplified/floppy_simpl4",
           "ntdrivers-simplified/kbfiltr_simpl1",
           "ntdrivers-simplified/kbfiltr_simpl2"

            ]
$subjects=["factorization", 
           "find",
           "power",
           "strmat1",
           "strmat2",
           "textfmt",
           "triangle",

           "tcas_v1",
           "replace",
           "tot_info",
           "print_tokens",
           "print_tokens2",
           "schedule",
           "schedule2",

           "s3_clnt_1_true-unreach-call",
           "s3_clnt_3.cil_true-unreach-call_true-termination",
           "s3_srvr_1_true-unreach-call",
           "s3_srvr_2_true-unreach-call",
           "s3_srvr_6_true-unreach-call",
           "s3_srvr_7_true-unreach-call",
           "s3_srvr_8_true-unreach-call",
           "s3_srvr_10_false-unreach-call",
           "s3_srvr_11_false-unreach-call",
           "s3_srvr_12_false-unreach-call",
           "s3_srvr_13_false-unreach-call",
           "s3_srvr_14_false-unreach-call",
           "s3_srvr_1a_true-unreach-call",
           "s3_srvr_1b_true-unreach-call_false-termination",

           "cdaudio_simpl1_false-unreach-call_true-termination",
           "diskperf_simpl1_true-unreach-call_true-termination",
           "floppy_simpl3_true-unreach-call_true-termination",
           "floppy_simpl4_true-unreach-call_true-termination",
           "kbfiltr_simpl1_true-unreach-call_true-termination",
           "kbfiltr_simpl2_true-unreach-call_true-termination"
            
        
            ]
$entry_functions=["factorization", 
                  "find",
                  "power",
                  "stringmatch1",
                  "stringmatch2",
                  "textfmt_main",
                  "triangle",

                  "alt_sep_test",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",

                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",

                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",
                  "test_main",

                ]

$max_execution_time=["300","300","300","300","300","300","300",
                     "300","300","300","300","300","300", "300",
                    "300","300","300","300","300","300", "300","300","300","300","300","300","300", "300",
                    "300","300","300","300","300","300"]

# only for CBMC's --unwind and --depth parameters
$max_unwind=["10","10","10","10","10","10","2",
             "10","10","20","12","12", "5", "5",
             #"10","10","10","10","10","10","10",
             "10","10","10","10","10","10","10", "10","10","10","10","10","10","10",
             "10","10","10","10","10","10"]

$max_depth=["120","120","120","120","120","120","40",
            "300","350","1000","450","450", "350", "250",
            "500"]

# the dir that contains all benchmark programs
$caut_ben="/home/lab301/mount_caut/caut/tingsucaut/benchmarks/"

$main_function=["testme", "testme", "testme", "testme", "testme", "testme", "testme",

                "testme", "test_main","test_main", "test_main", "test_main", "test_main", "test_main",

                "test_main", "test_main","test_main", "test_main", "test_main","test_main","test_main", "test_main","test_main","test_main", "test_main","test_main","test_main", "test_main",

                "test_main", "test_main", "test_main", "test_main", "test_main", "test_main"
                ]   


def get_def_use_pair_cnt(tool, subject_index)

    subject_loc=$subjects_loc[subject_index]
    subject = $subjects[subject_index]
    entry_func=$entry_functions[subject_index]
    puts "subject name: #{subject}, entry func: #{entry_func}"
    puts "skip to benchmark dir: #{$caut_ben}"
    def_use_pair_cnt = 0
    Dir.chdir($caut_ben) do

        cmd=""
        if tool.eql?("cbmc") || tool.eql?("cbmc-all") then
            usedef_file = subject_loc + "/" + "obj-cbmc-data" + "/" +      subject + "_cbmc.i.usedef.txt"
            `rm -rf #{usedef_file}`
            if not File.exist?(usedef_file) then
                cmd="bash caut.sh #{subject_loc}/#{subject}_cbmc.c #{entry_func} cbmc_df_test 0"
                puts "$ #{cmd}"
                `#{cmd}`
            end
        elsif tool.eql?("blast-old") then
            usedef_file = subject_loc + "/" + "obj-blast-old-data" + "/" + subject + "_blast.i.usedef.txt"
            `rm -rf #{usedef_file}`
            if not File.exist?(usedef_file) then
                cmd="bash caut.sh #{subject_loc}/#{subject}_blast.c #{entry_func} blast_old_df_test 0"
                puts "$ #{cmd}"
                `#{cmd}`
            end
        elsif tool.eql?("blast") then
            usedef_file = subject_loc + "/" + "obj-blast-data" + "/" +     subject + "_blast.i.usedef.txt"
            `rm -rf #{usedef_file}`
            if not File.exist?(usedef_file) then
                cmd="bash caut.sh #{subject_loc}/#{subject}_blast.c #{entry_func} blast_df_test 0"
                puts "$ #{cmd}"
                `#{cmd}`
            end
        elsif tool.eql?("cpachecker") then
            usedef_file = subject_loc + "/" + "obj-cpa-data" + "/" +       subject + "_cpa.i.usedef.txt"
            `rm -rf #{usedef_file}`
            if not File.exist?(usedef_file) then
                cmd="bash caut.sh #{subject_loc}/#{subject}_cpa.c #{entry_func} cpa_df_test 0"
                puts "$ #{cmd}"
                `#{cmd}`
            end
        end
        
        usedef_file=""
        if tool.eql?("cbmc") || tool.eql?("cbmc-all") then
            usedef_file= subject_loc + "/" + "obj-cbmc-data" + "/" +  subject + "_cbmc.i.usedef.txt"
        elsif tool.eql?("blast-old") then
            usedef_file= subject_loc + "/" + "obj-blast-old-data" + "/" + subject + "_blast.i.usedef.txt"
        elsif tool.eql?("blast") then
            usedef_file= subject_loc + "/" + "obj-blast-data" + "/" + subject + "_blast.i.usedef.txt"
        elsif tool.eql?("cpachecker") then
            usedef_file= subject_loc + "/" + "obj-cpa-data" + "/" + subject + "_cpa.i.usedef.txt"
        end
        puts "the use def file: #{usedef_file}"
        def_use_pair_cnt = `wc -l #{usedef_file}`.strip.to_i
        if def_use_pair_cnt <= 0  then
            puts "Error: fail to get the def-use pairs cnt"
            exit
        end
    end
    return def_use_pair_cnt

end

def instrument_program(tool, subject_index)

    pair_cnt = get_def_use_pair_cnt(tool, subject_index)
    subject_loc = $subjects_loc[subject_index]
    subject = $subjects[subject_index]
    entry_func=$entry_functions[subject_index]

    Dir.chdir($caut_ben) do
        
        cmd = ""
        if tool.eql?("cbmc") then
            cmd = "bash caut.sh #{subject_loc}/#{subject}_cbmc.c #{entry_func} cbmc_df_test #{pair_cnt}"
        elsif tool.eql?("cbmc-all") then # test all pairs at one time by cbmc, #pair_cnt actually not required
            cmd = "bash caut.sh #{subject_loc}/#{subject}_cbmc.c #{entry_func} cbmc_all_df_test #{pair_cnt}"
        elsif tool.eql?("blast-old") then
            cmd = "bash caut.sh #{subject_loc}/#{subject}_blast.c #{entry_func} blast_old_df_test #{pair_cnt}"
        elsif tool.eql?("blast") then
            cmd = "bash caut.sh #{subject_loc}/#{subject}_blast.c #{entry_func} blast_df_test #{pair_cnt}"
        elsif tool.eql?("cpachecker") then
            cmd = "bash caut.sh #{subject_loc}/#{subject}_cpa.c #{entry_func} cpa_df_test #{pair_cnt}"
        end
        puts "$ #{cmd}"
        # output the info at runtime
        IO.popen(cmd).each do |line|
            puts line
        end
   
        if tool.eql?("cbmc") then
            cmd = "ls #{subject_loc}/obj-cbmc/*.c | wc -l"
        elsif tool.eql?("cbmc-all") then
            cmd = "ls #{subject_loc}/obj-cbmc-all/*.c | wc -l"
        elsif tool.eql?("blast-old") then
             cmd = "ls #{subject_loc}/obj-blast-old/*.c | wc -l"
        elsif tool.eql?("blast") then
            cmd = "ls #{subject_loc}/obj-blast/*.c | wc -l"
        elsif tool.eql?("cpachecker") then
            cmd = "ls #{subject_loc}/obj-cpa/*.c | wc -l"
        end
        puts "$ #{cmd}"
        instrumented_files_cnt = `#{cmd}`.strip.to_i
        if pair_cnt == instrumented_files_cnt || instrumented_files_cnt == 1 then
            # when using "cbmc-all", instrumented_files_cnt = 1
        else
            puts "Error: instrumented files not correct!"
            exit
        end
    end
    
    return pair_cnt
end

def run_df_testing(tool, subject_index)

    pair_cnt = instrument_program(tool, subject_index)
    subject_loc = $subjects_loc[subject_index]
    subject = $subjects[subject_index]
    puts "there are total #{pair_cnt} def-use pairs in #{subject}"
    
    max_coverage_rate = 0.0
    current_coverage_rate = 0.1
    max_depth = $max_depth[subject_index].to_i

    #while current_coverage_rate > max_coverage_rate do
        
        max_coverage_rate = current_coverage_rate
        max_depth = max_depth + 1

    cmd = ""
    if tool.eql?("cbmc") then
        # run cbmc for data flow testing
        cmd = "bash run_cbmc.sh run_cbmc_df_testing #{$caut_ben}/#{subject_loc}/obj-cbmc/#{subject}_cbmc.c #{$main_function[subject_index]} #{pair_cnt} #{$max_execution_time[subject_index]} #{$max_unwind[subject_index]} #{max_depth}"
    elsif tool.eql?("cbmc-all") then
        # run cbmc for data flow testing with all duas
        cmd = "bash run_cbmc.sh run_cbmc_all_df_testing #{$caut_ben}/#{subject_loc}/obj-cbmc-all/#{subject}_cbmc.c #{$main_function[subject_index]}   #{pair_cnt} #{$max_execution_time[subject_index]} #{$max_unwind[subject_index]} #{max_depth}"
    elsif tool.eql?("blast-old") then
        cmd = "bash run_blast.sh run_blast_df_testing #{$caut_ben}/#{subject_loc}/obj-blast-old/#{subject}_blast.c          #{$main_function[subject_index]} #{pair_cnt} #{$max_execution_time[subject_index]} old"
    elsif tool.eql?("blast") then
        cmd = "bash run_blast.sh run_blast_df_testing #{$caut_ben}/#{subject_loc}/obj-blast/#{subject}_blast.c #{$main_function[subject_index]} #{pair_cnt} #{$max_execution_time[subject_index]} new"
    elsif tool.eql?("cpachecker") then
        cmd = "bash run_cpa.sh run_cpa_df_testing #{$caut_ben}/#{subject_loc}/obj-cpa/#{subject}_cpa.c #{$main_function[subject_index]} #{pair_cnt} #{$max_execution_time[subject_index]}"
    end

    puts "$ #{cmd}"
    # execute the cmd and output the running info.
    IO.popen(cmd).each do |line|
        puts line
    end

    if tool.eql?("cbmc") then
        # analyze the results
        cmd = "bash run_cbmc.sh analyze_cbmc_df_testing #{$caut_ben}/#{subject_loc}/obj-cbmc/#{subject}_cbmc.c #{$main_function[subject_index]}  #{pair_cnt} #{$max_execution_time[subject_index]} #{$max_unwind[subject_index]} #{max_depth}"
    elsif tool.eql?("cbmc-all") then
        cmd = "bash run_cbmc.sh analyze_cbmc_all_df_testing #{$caut_ben}/#{subject_loc}/obj-cbmc-all/#{subject}_cbmc.c #{$main_function[subject_index]}  #{pair_cnt} #{$max_execution_time[subject_index]} #{$max_unwind[subject_index]} #{max_depth}"
    elsif tool.eql?("blast-old") then
        cmd = "bash run_blast.sh analyze_blast_df_testing #{$caut_ben}/#{subject_loc}/obj-blast-old/#{subject}_blast.c             #{$main_function[subject_index]}  #{pair_cnt} #{$max_execution_time[subject_index]} old"
    elsif tool.eql?("blast") then
        cmd = "bash run_blast.sh analyze_blast_df_testing #{$caut_ben}/#{subject_loc}/obj-blast/#{subject}_blast.c #{$main_function[subject_index]}  #{pair_cnt} #{$max_execution_time[subject_index]} new"
    elsif tool.eql?("cpachecker") then
        cmd = "bash run_cpa.sh analyze_cpa_df_testing #{$caut_ben}/#{subject_loc}/obj-cpa/#{subject}_cpa.c #{$main_function[subject_index]}  #{pair_cnt} #{$max_execution_time[subject_index]}"
    end

    puts "$ #{cmd}"
    # execute the cmd and output the running info.
    IO.popen(cmd).each do |line|
        puts line
        if line.start_with?("df_coverage") then
            `echo -n "#{subject}: #{line}" >> ./results/#{tool}.result.txt`
            `echo "-------\n" >> ./results/#{tool}.result.txt`
            current_coverage = line.split(":")[1].strip.to_f
            puts "the current coverage: #{current_coverage}"
        elsif line.start_with?("statistics") || line.start_with?("data flow coverage") then
            `echo -n "#{subject}: #{line}" >> ./results/#{tool}.result.txt`
        end
    end

    #end


    
    # dump the results to cvs
    pwd = `pwd`.strip
    result_file = ""
    original_result_file = ""
    use_def_file = ""
    if tool.eql?("cbmc") then
        result_file = pwd + "/results/" + subject + "_cbmc_5.6.csv"
        original_result_file = subject_loc + "/obj-cbmc/" + "cbmc.df." + subject + "_cbmc.result"
        use_def_file = subject_loc + "/obj-cbmc-data/" + subject + "_cbmc.i.usedef.txt"

    elsif tool.eql?("cbmc-all") then
        result_file = pwd + "/results/" + subject + "_cbmc_5.6_all_pair_testing.csv"
        original_result_file = subject_loc + "/obj-cbmc-all/" + "cbmc.df." + subject + "_cbmc.result"
        # sort the lines in the result file by the dua id in ascending order
        cmd = "cat #{original_result_file} | sort -g -o #{original_result_file}"
        puts "$ #{cmd}"
        `#{cmd}`
        use_def_file = subject_loc + "/obj-cbmc-data/" + subject + "_cbmc.i.usedef.txt"

    elsif tool.eql?("blast-old") then
        
        result_file = pwd + "/results/" + subject + "_blast_2.5.csv"
        original_result_file = subject_loc + "/obj-blast-old/" + "blast.2.5.df." + subject + "_blast.result"
        use_def_file = subject_loc + "/obj-blast-old-data/" + subject + "_blast.i.usedef.txt"
        
    elsif tool.eql?("blast") then
        result_file = pwd + "/results/" + subject + "_blast_2.7.3.csv"
        original_result_file = subject_loc + "/obj-blast/" + "blast.2.7.3.df." + subject + "_blast.result"
        use_def_file = subject_loc + "/obj-blast-data/" + subject + "_blast.i.usedef.txt"

    elsif tool.eql?("cpachecker") then
        result_file = pwd + "/results/" + subject + "_cpa_1.6.1.csv"
        original_result_file = subject_loc + "/obj-cpa/" + "cpa.1.6.1.df." + subject + "_cpa.result"
        use_def_file = subject_loc + "/obj-cpa-data/" + subject + "_cpa.i.usedef.txt"
    end
    `rm -rf #{result_file}` # remove old file
    Dir.chdir($caut_ben) do
        puts "original result file: #{original_result_file}"
        puts "use def file: #{use_def_file}"
        open(result_file, "a") { |f|
            file_2 = File.open(original_result_file).to_enum
            file_1 = File.open(use_def_file).to_enum
            loop do
               line_1 = file_1.next.strip
               first_space_loc = line_1.index(" ")
               dua_id = line_1[0..first_space_loc-1]
               dua_info = line_1[(first_space_loc+1)..(line_1.length-1)]
               line_2 = file_2.next.strip
               res = line_2.split(" ")
               f.puts subject + "," + dua_id + "," + dua_info + "," + res[1] + "," + res[2] + "\n"
            end
        }
    end

end

def test_subjects(tool)


    #for index in 22..$subjects.length-1 do
    for index in $subject_id..$subject_id do

        if tool.eql?("cbmc") then
            puts "run cbmc (v5.6) for #{$subjects[index]}"
            run_df_testing(tool, index)
        elsif tool.eql?("cbmc-all") then
            puts "run cbmc (v5.6) for #{$subjects[index]} with all duas"
            run_df_testing(tool, index)
        elsif tool.eql?("blast-old") then
            puts "run blast old version (v2.5) for #{$subjects[index]}"
            run_df_testing(tool, index)
        elsif tool.eql?("blast") then
            puts "run blast latest version (v2.7.3) for #{$subjects[index]}"
            run_df_testing(tool, index)
        elsif tool.eql?("cpachecker") then
            puts "run cpachecker (v1.6.1) for #{$subjects[index]}"
            run_df_testing(tool, index)
        else
            puts "This script runs model checkers for data flow testing"
            puts "Usage: ruby #this_file.rb #tool_name"
            puts "now supports these tools: cbmc, cbmc-all, blast, blast-old , cpachecker"
            puts "cbmc: run cbmc for one pair, cbmc-all: run cbmc for all pairs\nblast: run the latest version of blast, blast-old: run an early stable version of blast, both are for one pair \ncpachecker: run cpachecker for one pair"
            exit
        end
    end

end

tool=ARGV[0]
$subject_id=ARGV[1].to_i
test_subjects(tool)

