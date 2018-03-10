#!/usr/bin/env ruby
require 'csv'

# This script cross-checks the results between KLEE and model checkers, and those of model checkers themselves
# The conflicting scenarios: (1) KLEE says feasible, but model checkers say infeasible; (2) one model checker says feasible, while the other says infeasible

subjects  = ["dft_previous_research_benchmark/factorization", # benchmark for previous dft research work
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
		       #"ssh-simplified/s3_srvr_1",
		       "ssh-simplified/s3_srvr_2",
		       #"ssh-simplified/s3_srvr_6",
		       "ssh-simplified/s3_srvr_7",
		       "ssh-simplified/s3_srvr_8",
		       "ssh-simplified/s3_srvr_10",
		       #"ssh-simplified/s3_srvr_11",
		       "ssh-simplified/s3_srvr_12",
		       "ssh-simplified/s3_srvr_13",
		       #"ssh-simplified/s3_srvr_14",
		       "ssh-simplified/s3_srvr_1a",
		       "ssh-simplified/s3_srvr_1b",

		       "ntdrivers-simplified/cdaudio_simpl1",
		       "ntdrivers-simplified/diskperf_simpl1",
		       "ntdrivers-simplified/floppy_simpl3",
		       "ntdrivers-simplified/floppy_simpl4",
		       "ntdrivers-simplified/kbfiltr_simpl1",
		       "ntdrivers-simplified/kbfiltr_simpl2"
             
]


subject_names = [ "factorization",
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
                  #"s3_srvr_1_true-unreach-call",
                  "s3_srvr_2_true-unreach-call",
                  #"s3_srvr_6_true-unreach-call",
                  "s3_srvr_7_true-unreach-call",
                  "s3_srvr_8_true-unreach-call",
                  "s3_srvr_10_false-unreach-call",
                  #"s3_srvr_11_false-unreach-call",
                  "s3_srvr_12_false-unreach-call",
                  "s3_srvr_13_false-unreach-call",
                  #"s3_srvr_14_false-unreach-call",
                  "s3_srvr_1a_true-unreach-call",
                  "s3_srvr_1b_true-unreach-call_false-termination",

                  "cdaudio_simpl1_false-unreach-call_true-termination",
                  "diskperf_simpl1_true-unreach-call_true-termination",
                  "floppy_simpl3_true-unreach-call_true-termination",
                  "floppy_simpl4_true-unreach-call_true-termination",
                  "kbfiltr_simpl1_true-unreach-call_true-termination",
                  "kbfiltr_simpl2_true-unreach-call_true-termination"
]

class CoverageData

    # tool name
    @tool_name

    # dua id
    @dua_id

    # status (true or false)
    @covered

    # testing time in seconds (maximum 300s)
    @testing_time

    def initialize(id, covered, time, tool)
        @tool_name = tool
        @dua_id = id
        if tool.eql?("klee") then
           if covered.eql?("1")
              @covered = "unsafe"
              @testing_time = time.to_f()
           else
              @covered = "unknown"
              @testing_time = 300.00
           end
         elsif tool.eql?("cbmc") || tool.eql?("cpachecker") || tool.eql?("blast") then
              if covered.eql?("unsafe") then
                 @covered = "unsafe"
                 @testing_time = time.to_f()
              elsif covered.eql?("safe") then
                 @covered = "safe"
                 @testing_time = time.to_f()
              else
                 @covered = "unknown"
                 @testing_time = 300.00
              end
         end
    end

    def get_dua_id()
        @dua_id
    end

    def get_covered_status()
        @covered
    end

end

class MapData

    # dua id of KLEE
    @klee_dua_id

    # dua id of Model Checker
    @mc_dua_id

    def initialize(klee_id, mc_id)
        @klee_dua_id, @mc_dua_id = klee_id, mc_id
    end
     
    def get_klee_dua_id()
        @klee_dua_id
    end
        
    def get_mc_dua_id()
        @mc_dua_id
    end

end

# get the mapping between klee and mc pairs
def get_map_data(map_data_file)
     
    dua_mapping = []
    CSV.foreach(map_data_file) do |row|
                   
        map_data = MapData.new(row[0], row[1])
        dua_mapping.push(map_data)
                                 
    end
               
    return dua_mapping
                    
end

def get_klee_data(klee_data_file)
    klee_data = []

    CSV.foreach(klee_data_file) do |row|
        if row[0].eql?("name") then # skip header
            next
        end
        coverage_data = CoverageData.new(row[2], row[3], row[4], "klee")
        klee_data.push(coverage_data)
    end

    return klee_data
end

def get_mc_data(mc_data_file, tool_name)
    mc_data = []
    File.open(mc_data_file).each_line{ |line|
        values = line.split(" ")
        coverage_data = CoverageData.new(values[0].strip(), values[2].strip(), values[1].strip(), tool_name)
        mc_data.push(coverage_data)

    }
    return mc_data
end

def map_klee_pair_id_to_mc_pair_id(klee_pair_id, dua_mapping)
    is_found = false

    for map in dua_mapping do
        if map.get_klee_dua_id.eql?(klee_pair_id) then
            is_found = true
            return map.get_mc_dua_id
        end
    end

    if is_found == false then
        return "-1"
    end
end

def cross_check_klee_and_mc(klee_data, mc_data, map_data, mc_tool_name, subject, strategy)


	conflict_pairs_file = "/home/lab301/dft-scripts/tests/se_mc_data/se_data/#{subject}/#{subject}_#{strategy}_conflict_pairs.csv"
    
    if File.exist?(conflict_pairs_file) then
        `rm -rf #{conflict_pairs_file}`
    end

	conflict_pairs = []

    for klee_pair in klee_data do

        klee_pair_id = klee_pair.get_dua_id()
        klee_pair_coverage_status = klee_pair.get_covered_status()
        pair_id = map_klee_pair_id_to_mc_pair_id(klee_pair_id, map_data)

        if pair_id.eql?("-1") then
            next
        end

        for mc_pair in mc_data do

            mc_pair_id = mc_pair.get_dua_id()
            if pair_id.eql?(mc_pair_id) then
                
                mc_pair_coverage_status = mc_pair.get_covered_status()
                if klee_pair_coverage_status.eql?("unsafe") && mc_pair_coverage_status.eql?("safe") then
					# dump to a file
					open(conflict_pairs_file, "a") { |f|
        				f.puts "#{klee_pair_id}"
    				}
					conflict_pairs.push(klee_pair_id)
                    puts "#{klee_pair_id}"
                end

            end

        end

    end

	return conflict_pairs
	
end

def remove_conflict_pairs(klee_data_file, conflict_pairs, subject, strategy)

	klee_fixed_file = "/home/lab301/dft-scripts/tests/se_mc_data/se_data/#{subject}/#{subject}_#{strategy}_fixed.csv"

    if File.exist?(klee_fixed_file) then
        `rm #{klee_fixed_file}`
    end

	CSV.foreach(klee_data_file) do |row|
        if row[0].eql?("name") then # skip header
            next
        end

        if row[3].eql?("0") then # skip uncovered pairs
            next
        end
		
		pair_id = row[2]
        
		if not conflict_pairs.include?(pair_id) then
			# dump to a file
			open(klee_fixed_file, "a") { |f|
        		f.puts "#{row[0]},#{row[1]},#{row[2]},#{row[3]},#{row[4]},#{row[5]},#{row[6]},#{row[7]}"
    		}
		end
    end

end

for i in 0..(subjects.size()-1) do

    subject =  subject_names[i]
    subject_path = subjects[i]
    puts subject

	strategies = ["dfs", "rss", "md2u", "sdgs", "cpgs"]
    #strategies = ["cpgs"]
	
	for strategy in strategies do

		# klee data file
		klee_data_file="/home/lab301/dft-scripts/tests/se_mc_data/se_data/#{subject}/#{subject}_#{strategy}" + ".csv"

		# leverage cpachecker data file
		cpachecker_data_file = "/home/lab301/mount_caut/caut/tingsucaut/benchmarks/#{subject_path}" +  "/obj-cpa/cpa.1.6.1.df.#{subject}" + "_cpa.result"

		# map data file
		map_data_file= "/home/lab301/dft-scripts/tests/se_mc_data/se_mc_pair_map/benchmarks/#{subject}" + "_match.i.usedef.txt"

		klee_data = get_klee_data(klee_data_file)
		mc_data = get_mc_data(cpachecker_data_file, "cpachecker")
		map_data = get_map_data(map_data_file)

		conflict_pairs = cross_check_klee_and_mc(klee_data, mc_data, map_data, "cpachecker", subject, strategy)

		remove_conflict_pairs(klee_data_file, conflict_pairs, subject, strategy)


	end
end

