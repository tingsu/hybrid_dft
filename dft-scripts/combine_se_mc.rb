#! /usr/bin/env ruby
require 'csv'

# This script computes the combination of symbolic execution and model checking for data flow testing
# Input file: for each subject, klee_data, mc_data, pair_mapping_data
# klee_data and mc_data can contain the results of covered/uncovered pairs

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
			@testing_time = 300 # fix this bug, should use the actual testing time, do not set it as 300
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
			@testing_time = 300
		end
	end
  end

  def get_testing_time() 
	@testing_time
  end

  def get_dua_id()
	@dua_id
  end

  def get_covered_status()
	@covered
  end
    
  def output_coverage_data()
    puts "Tool(#{@tool_name}), Id(#{@dua_id}), Time(#{@testing_time}), Status(#{@covered})"
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


# get klee testing data
def get_klee_data(klee_data_file) 

	klee_data = []

	CSV.foreach(klee_data_file) do |row|

	  if row[0].eql?("name") then # skip header
		next
	  end

	  coverage_data = CoverageData.new(row[2], row[3], row[4], "klee")
	  klee_data.push(coverage_data)
	end

    puts "the size of klee data: #{klee_data.size()}"

	return klee_data
end

# get cbmc testing data
def get_mc_data(mc_data_file, tool_name)

	mc_data = []

	File.open(mc_data_file).each_line{ |line|

	  values = line.split(" ")

	  coverage_data = CoverageData.new(values[0].strip(), values[2].strip(), values[1].strip(), tool_name)


	  mc_data.push(coverage_data)
	}
	
	return mc_data
end

# get the mapping between klee and mc pairs
def get_map_data(map_data_file)

    common_pair_cnt = 0
	
	dua_mapping = []
	CSV.foreach(map_data_file) do |row|
	  
      
	  map_data = MapData.new(row[0], row[1])
	  dua_mapping.push(map_data)

      if not row[1].eql?("no") then
          common_pair_cnt = common_pair_cnt + 1
      end

	end

	return dua_mapping, common_pair_cnt

end


def map_klee_pair_id_to_mc_pair_id(klee_pair_id, dua_mapping) 

	is_found = false

	for map in dua_mapping do
	
		if map.get_klee_dua_id.eql?(klee_pair_id) then
			is_found = true
            if map.get_mc_dua_id.eql?("no") then
                #puts "this is no!!! " # fix bug! a klee pair may not be able to map to mc pair
                return "-1"
            end
			return map.get_mc_dua_id
		end

	end

	if is_found == false then
		return "-1"
	end

end

def map_mc_pair_id_to_klee_pair_id(mc_pair_id, dua_mapping)

    is_found = false

    for map in dua_mapping do

        if map.get_mc_dua_id.eql?(mc_pair_id) then
            is_found = true
            return map.get_klee_dua_id
        end
    end

    if is_found == false then
        return "-1"
    end

end


def output_execution_info(klee_pair_id, mc_pair_id, covered_status, testing_time, timeout)

	if timeout == true then
		puts "Unknown pair, KLEE(#{klee_pair_id}), MC(#{mc_pair_id}), Time(#{testing_time})"
		return
	end

	if covered_status.eql?("unsafe") then
		puts "Feasible pair, KLEE(#{klee_pair_id}), MC(#{mc_pair_id}), Time(#{testing_time})"
	elsif covered_status.eql?("safe") then
		puts "Infeasible pair, KLEE(#{klee_pair_id}), MC(#{mc_pair_id}), Time(#{testing_time})"
	end
end

# combine KLEE and SE

#sequential combine: execute KLEE on each pair for 300s, and then 
# execute a model checker on each pair for 300s
def sequential_combine(klee_data, cbmc_data, dua_mapping)

	# get the total pair cnt, the pair id [1..total_pair_cnt]	
	total_pair_cnt = cbmc_data.length()
	total_covered_pair_cnt = 0

end

# interleave combine: KLEE->MC->KLEE->MC-> ... 10s, 20s, 40s, 80s, 160s, 300s 
# 	start from 10s, and increase by 2*n
def interleave_combine(klee_data, mc_data, dua_mapping, mc_tool_name, common_pair_cnt)

	total_pair_cnt = common_pair_cnt
	puts "total pair cnt: #{total_pair_cnt}"
	total_covered_pair_cnt = 0
	total_infeasible_pair_cnt = 0 
	total_testing_time = 0.0
	concluded_pairs = [] # record covered pairs' and infeasible pairs' ids, Note we stores the mc version id in this list!!

	#time_bounds = [10, 20, 40, 80, 160, 300]
    time_bounds = [10, 30, 90, 300]
    #time_bounds = [30, 90, 180, 300]
    #time_bounds = [50, 100, 200, 300]
	for i in time_bounds do

		puts "---Run Klee, Time Bound (#{i})---"
		# run klee
		for klee_pair in klee_data do

			# get pair id
			klee_pair_id = klee_pair.get_dua_id()
			# map the klee pair id to mc pair id
			pair_id = map_klee_pair_id_to_mc_pair_id(klee_pair_id, dua_mapping)


			# if the pair has been concluded OR the pair only belongs to klee, omit them
			if concluded_pairs.include?(pair_id) || pair_id.eql?("-1") then
				next
			end


			# get testing time
			testing_time = klee_pair.get_testing_time()

			if testing_time < i then

				concluded_pairs.push (pair_id)
			
				# increase testing time
				total_testing_time = total_testing_time + testing_time	

				# check covered status				
				covered_status = klee_pair.get_covered_status()


				if covered_status.eql?("unsafe") then
					total_covered_pair_cnt = total_covered_pair_cnt + 1
                    #puts "Id(#{pair_id}) is unsafe, covered by KLEE"
				elsif covered_status.eql?("safe") then
                    #puts "Id(#{pair_id}) is safe, concluded by KLEE"
					total_infeasible_pair_cnt = total_infeasible_pair_cnt + 1
				end

				#output_execution_info(klee_pair_id, pair_id, covered_status, testing_time, false)

			else
		
				# increase testing time
				total_testing_time = total_testing_time + i
				#output_execution_info(klee_pair_id, pair_id, covered_status, i, true)
			end
			
		end

		
		puts "---Run #{mc_tool_name}, Time Bound (#{i})---"
		# run mc
		for mc_pair in mc_data do


			# get pair id
			pair_id = mc_pair.get_dua_id()
            klee_pair_id = map_mc_pair_id_to_klee_pair_id(pair_id, dua_mapping)

			
			# if the pair has been concluded, omit them (Here, we include those pairs that only belong to MC)
			if concluded_pairs.include?(pair_id) || klee_pair_id.eql?("-1") then
				next
			end			

			testing_time = mc_pair.get_testing_time()

			if testing_time < i then


				concluded_pairs.push (pair_id)
			
				# increase testing time
				total_testing_time = total_testing_time + testing_time	

				# check covered status				
				covered_status = mc_pair.get_covered_status()
				if covered_status.eql?("unsafe") then
                    #puts "Id(#{pair_id}) is unsafe, covered by #{mc_tool_name}"
					total_covered_pair_cnt = total_covered_pair_cnt + 1
				elsif covered_status.eql?("safe") then
                    #puts "Id(#{pair_id}) is safe, concluded by #{mc_tool_name}"
					total_infeasible_pair_cnt = total_infeasible_pair_cnt + 1
				end				

				#output_execution_info(klee_pair_id, pair_id, covered_status, testing_time, false)
			else
		
				# increase testing time
				total_testing_time = total_testing_time + i
				#output_execution_info(klee_pair_id, pair_id, covered_status, i, true)
			end
		end
	end

	coverage_percentage = ((total_covered_pair_cnt)*1.0/(total_pair_cnt-total_infeasible_pair_cnt))*100
	ret = "Coverage(Klee+#{mc_tool_name}): #{coverage_percentage}%, total(#{total_pair_cnt}), covered/infeasible/unknown(#{total_covered_pair_cnt}/#{total_infeasible_pair_cnt}/#{total_pair_cnt - total_covered_pair_cnt - total_infeasible_pair_cnt}), Total time: #{total_testing_time}(s)"
    return ret, coverage_percentage, total_testing_time
end

def run_klee(klee_data, klee_def_use_file, dua_mapping, common_pair_cnt)

	klee_pair_cnt = common_pair_cnt

	covered_pair_cnt = 0
	total_testing_time = 0.0
	coverage_percenrage = 0.0	

	for klee_pair in klee_data do

        klee_pair_id = klee_pair.get_dua_id()
        if map_klee_pair_id_to_mc_pair_id(klee_pair_id, dua_mapping).eql?("-1") then
            next
        end
        # check covered status              
        covered_status = klee_pair.get_covered_status()
        if covered_status.eql?("unsafe") then
		    total_testing_time = klee_pair.get_testing_time()
		    covered_pair_cnt = covered_pair_cnt + 1
        end
	end

	total_testing_time = total_testing_time + (klee_pair_cnt - covered_pair_cnt)*300
	coverage_percentage = (covered_pair_cnt*1.0/klee_pair_cnt)*100.0
	ret = "Coverage(Klee): #{coverage_percentage}%, total(#{klee_pair_cnt}), covered/unknown(#{covered_pair_cnt}/#{klee_pair_cnt - covered_pair_cnt}), Total time: #{total_testing_time}(s)"
    return ret, coverage_percentage, total_testing_time
end

def run_mc(mc_data, mc_tool_name,dua_mapping, common_pair_cnt)

	mc_pair_cnt = common_pair_cnt
	
	covered_pair_cnt = 0
	infeasible_pair_cnt = 0
	total_testing_time = 0.0
	coverage_percenrage = 0.0

 	for mc_pair in mc_data do

        mc_pair_id = mc_pair.get_dua_id()
        if map_mc_pair_id_to_klee_pair_id(mc_pair_id, dua_mapping).eql?("-1") then
            next
        end

		covered_status = mc_pair.get_covered_status()
		testing_time = 0.0
		if covered_status.eql?("unsafe") then
			covered_pair_cnt = covered_pair_cnt + 1
			testing_time = mc_pair.get_testing_time()
		elsif covered_status.eql?("safe") then
			infeasible_pair_cnt = infeasible_pair_cnt + 1
			testing_time = mc_pair.get_testing_time()
		else
			testing_time = 300.0
		end
		total_testing_time = total_testing_time + testing_time
	end
	coverage_percentage = (covered_pair_cnt*1.0/(mc_pair_cnt - infeasible_pair_cnt))*100.0
	ret ="Coverage(#{mc_tool_name}): #{coverage_percentage}%, total(#{mc_pair_cnt}), covered/infeasible/unknown(#{covered_pair_cnt}/#{infeasible_pair_cnt}/#{mc_pair_cnt - covered_pair_cnt - infeasible_pair_cnt}), Total time: #{total_testing_time}(s)"
    return ret, coverage_percentage, total_testing_time
end

# compute the common covered pairs, and exclusively covered pairs (including both feasible and infeasible pairs)
def compare_klee_and_mc_testing_results(klee_data, klee_def_use_file, mc_data, dua_mapping)

    pairs = []
    common_concluded_pair_cnt = 0
    klee_exclusive_concluded_pair_cnt = 0
    mc_exclusive_concluded_pair_cnt = 0
    conflict_pair_cnt = 0

    klee_pair_cnt = `wc -l #{klee_def_use_file} | cut -d "" -f 1`.strip().to_i()

    for mc_pair in mc_data do

       # get pair id
       mc_pair_id = mc_pair.get_dua_id()
       # map the mc pair id to klee pair id
       pair_id = map_mc_pair_id_to_klee_pair_id(mc_pair_id, dua_mapping)
       mc_pair_covered_status = mc_pair.get_covered_status()

       is_found = false

       for klee_pair in klee_data do

           klee_pair_id = klee_pair.get_dua_id()

           # common concluded feasible pair
           if pair_id.eql?(klee_pair_id) && mc_pair_covered_status.eql?("unsafe") then
                is_found = true
                common_concluded_pair_cnt += 1
                break
           elsif pair_id.eql?(klee_pair_id) && mc_pair_covered_status.eql?("unknown") then
                is_found = true
                klee_exclusive_concluded_pair_cnt += 1
           elsif pair_id.eql?(klee_pair_id) && mc_pair_covered_status.eql?("safe") then
                is_found = true
                conflict_pair_cnt += 1
           end

       end

       # not 
       if is_found == false && (not mc_pair_covered_status.eql?("unknown")) then
            mc_exclusive_concluded_pair_cnt += 1
       end

    end

    klee_exclusive_concluded_pair_cnt += klee_pair_cnt - common_concluded_pair_cnt

    return common_concluded_pair_cnt, klee_exclusive_concluded_pair_cnt, mc_exclusive_concluded_pair_cnt, conflict_pair_cnt

end


output_title = 1
if File.exists?("hybrid_approach.csv") then
    `rm hybrid_approach.csv`
end

for i in 0..(subject_names.size-1) do

    puts subject_names[i]

	# klee data file
	klee_data_file="/home/lab301/dft-scripts/tests/se_mc_data/se_data/" + subject_names[i] + "/" + subject_names[i] + "_cpgs_fixed.csv"
    if not File.exist?(klee_data_file) then
        puts "the klee data file does not exist!"
        klee_data_file="/home/lab301/dft-scripts/tests/se_mc_data/se_data/" + subject_names[i] + "/" + subject_names[i] + "_cpgs.csv"
    end
	klee_def_use_file="/home/lab301/dft-scripts/tests/se_mc_data/se_mc_pair_map/benchmarks/" + subject_names[i] + "_klee.i.usedef.txt"

    # cbmc data file
	cbmc_data_file= "/home/lab301/caut/tingsucaut/benchmarks/" + subjects[i] + "/obj-cbmc/cbmc.df." + subject_names[i] + "_cbmc.result"
    # cpachecker data file
    cpachecker_data_file = "/home/lab301/mount_caut/caut/tingsucaut/benchmarks/" + subjects[i] + "/obj-cpa/cpa.1.6.1.df." + subject_names[i] + "_cpa.result"
    # blast data file
    blast_data_file = "/home/lab301/mount_caut/caut/tingsucaut/benchmarks/" + subjects[i] + "/obj-blast/blast.2.7.3.df." +                subject_names[i] + "_blast.result"

    # map data file
	map_data_file= "/home/lab301/dft-scripts/tests/se_mc_data/se_mc_pair_map/benchmarks/" + subject_names[i] + "_match.i.usedef.txt"

    # get klee data
	klee_data = get_klee_data(klee_data_file)
    # get mc data 
	cbmc_data = get_mc_data(cbmc_data_file, "cbmc")
    cpachecker_data = get_mc_data(cpachecker_data_file, "cpachecker")
    blast_data = get_mc_data(blast_data_file, "blast")
    # get map data
	dua_mapping, common_pair_cnt = get_map_data(map_data_file)

    results = ""
    results += subject_names[i] + "\n"
    # combine klee and mc
    output, klee_cbmc_coverage_percentage, klee_cbmc_testing_time =  interleave_combine(klee_data, cbmc_data, dua_mapping, "cbmc", common_pair_cnt) 
    results += output + "\n"
    output, klee_cpachecker_coverage_percentage, klee_cpachecker_testing_time = interleave_combine(klee_data, cpachecker_data, dua_mapping, "cpachecker", common_pair_cnt) 
    results += output + "\n"
    output, klee_blast_coverage_percentage, klee_blast_testing_time = interleave_combine(klee_data, blast_data, dua_mapping, "blast", common_pair_cnt)
    results += output + "\n"

    # run klee and mc respectively
    output, klee_coverage_percentage, klee_testing_time = run_klee(klee_data, klee_def_use_file, dua_mapping, common_pair_cnt) 
    results += output + "\n"
    output, cbmc_coverage_percentage, cbmc_testing_time = run_mc(cbmc_data, "cbmc", dua_mapping, common_pair_cnt)
    results += output + "\n"
    output, cpachecker_coverage_percentage, cpachecker_testing_time =  run_mc(cpachecker_data, "cpachecker", dua_mapping, common_pair_cnt) 
    results += output + "\n"
    output, blast_coverage_percentage, blast_testing_time =  run_mc(blast_data, "blast", dua_mapping, common_pair_cnt) 
    results += output + "\n"
    puts results

    open("hybrid_approach.csv", "a") { |f|

        if output_title == 1 then
            output_title = 0 # disable output for next subject
            f.puts "subject,klee_coverage,blast_coverage,cpachecker_coverage,cbmc_coverage,klee_blast_coverage,klee_cpachecker_coverage,klee_cbmc_coverage, klee_time, blast_time, cpachecker_time, cbmc_time, klee_blast_time, klee_cpachecker_time, klee_cbmc_time"
        end
        f.puts "#{subject_names[i]}, #{klee_coverage_percentage}, #{blast_coverage_percentage}, #{cpachecker_coverage_percentage}, #{cbmc_coverage_percentage}, #{klee_blast_coverage_percentage}, #{klee_cpachecker_coverage_percentage}, #{klee_cbmc_coverage_percentage}, #{klee_testing_time}, #{blast_testing_time}, #{cpachecker_testing_time}, #{cbmc_testing_time}, #{klee_blast_testing_time}, #{klee_cpachecker_testing_time}, #{klee_cbmc_testing_time}"

    }

    common_pair_cnt, klee_pair_cnt, cbmc_pair_cnt, conflict_pair_cnt = compare_klee_and_mc_testing_results(klee_data, klee_def_use_file, cbmc_data, dua_mapping)

    
end




