#!/usr/bin/env python

import sys
import numpy as np
import matplotlib as mpl
mpl.use('agg')
import matplotlib.pyplot as plt

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

# pair_type: "feasible" or "infeasible"
def get_testing_time(data_file_path, mc_match_file_path, tool_name, subject_name, pair_type):

    # read the match file path
    mc_match_file = open(mc_match_file_path, "r")
    mc_match_lines = mc_match_file.readlines()
    cnt = len(mc_match_lines)
    i = 0
    match_pairs = []
    while (i<cnt):
        line = mc_match_lines[i]
        ids = line.split(',')
        #print line
        mc_id = ids[1].strip('\n')
        if mc_id == "no": # skip "no" keyword
            i = i+1
            continue
        match_pairs.append(mc_id)
        i =i+1

    # read the data file dumped by model checkers
    result_file = open(data_file_path, "r")
    result_lines = result_file.readlines()
    print "process %s tested by %s with %d lines, compute pair type of %s." % (subject_name, tool_name, len(result_lines), pair_type)
    cnt = len(result_lines)
    i = 0
    testing_time_list = []
    while (i<cnt):
        line = result_lines[i]
        nums = line.split(' ')
        #print nums

        # check whether the pair is in mc match list
        pair_id = nums[0].strip('\n')
        print 'pair id: %s #' % pair_id
        if not (pair_id in match_pairs):
            print 'this pair is not in klee'
            i = i + 1
            continue

        # remove the tailing newline
        pair_status = nums[2].strip('\n')
        #if not isinstance(nums[1],float):   # ensure this is a float number
        #    print 'this is not a float time, skip it'
        #    i = i + 1
        #    continue
        pair_time = float(nums[1])
        if pair_type == "feasible":
            #print 'target at feasible pairs, current pair: %s' % pair_status
            if (str(pair_status) == 'unsafe'):
                #print pair_time
                testing_time_list.append(pair_time)

        elif pair_type == "infeasible":
            #print 'target at infeasible pairs, current pair: %s' % pair_status
            if (pair_status == "safe"):
                #print pair_time
                testing_time_list.append(pair_time)
        #print float(nums[1])
        i = i+1

    print testing_time_list
    return testing_time_list

def plot_box(subject_name, collection_1, collection_2, collection_3):

    data_to_plot = [collection_1, collection_2, collection_3]

    # Create a figure instance
    fig = plt.figure(1, figsize=(9, 6))

    plt.ylim((0,5))

    # Create an axes instance
    ax = fig.add_subplot(111)

    # Create the boxplot
    bp = ax.boxplot(data_to_plot)

    # Save the figure
    fig_name = subject_name + ".png"
    fig.savefig(fig_name, bbox_inches='tight')

# the random data
#np.random.seed(10)
#collectn_1 = np.random.normal(100, 10, 200)
#collectn_2 = np.random.normal(80, 30, 200)
#collectn_3 = np.random.normal(90, 20, 200)
#collectn_4 = np.random.normal(70, 25, 200)

subjects_cnt = len(subjects)
i = 0
print 'there are total %d subjects, now compute statistics for %s pairs' % (subjects_cnt, sys.argv[1])
fo = open("mc_testing_time_" + sys.argv[1] + ".csv", "a")

while (i<subjects_cnt):

    # the klee & mc match file
    mc_match_file_path = "/home/lab301/dft-scripts/tests/se_mc_data/se_mc_pair_map/benchmarks/" + subject_names[i] + "_match.i.usedef.txt"

    # get cpa result
    cpa_data_file_path = "/home/lab301/mount_caut/caut/tingsucaut/benchmarks/" + subjects[i] + "/obj-cpa/cpa.1.6.1.df." + subject_names[i] + "_cpa.result"

    cpa_collection = get_testing_time(cpa_data_file_path, mc_match_file_path, 'cpachecker', subject_names[i], sys.argv[1])

    collection_size = len(cpa_collection)

    if not (len(cpa_collection) == 0):
        Q1, median, Q3 = np.percentile(np.asarray(cpa_collection), [25, 50, 75])
        SIQR = (Q3-Q1)*1.0/2
        print '%s-%s, %d, %f, %f, %f, %f' % (subject_names[i], sys.argv[1], collection_size, Q1, median, Q3, SIQR)
        out = subject_names[i] + "-" + sys.argv[1] + ",cpachecker," + str(collection_size) + "," + str(Q1) + "," + str(median) + "," + str(Q3) + "," + str(SIQR)
        fo.write(out)
        fo.write("\n")

    # get blast result
    blast_data_file_path =  "/home/lab301/mount_caut/caut/tingsucaut/benchmarks/" + subjects[i] + "/obj-blast/blast.2.7.3.df." + subject_names[i] + "_blast.result"
    blast_collection = get_testing_time(blast_data_file_path, mc_match_file_path, 'blast', subject_names[i], sys.argv[1])

    collection_size = len(blast_collection)

    if (not (len(blast_collection) == 0)):
        Q1, median, Q3 = np.percentile(np.asarray(blast_collection), [25, 50, 75])
        SIQR = (Q3-Q1)*1.0/2
        print '%s-%s, %d, %f, %f, %f, %f' % (subject_names[i], sys.argv[1], collection_size, Q1, median, Q3, SIQR)
        out = subject_names[i] + "-" + sys.argv[1] + ",blast," + str(collection_size) + "," + str(Q1) + "," + str(median) + "," + str(Q3) + "," + str(SIQR)
        fo.write(out)
        fo.write("\n")

    # get cbmc result
    cbmc_data_file_path =  "/home/lab301/mount_caut/caut/tingsucaut/benchmarks/" + subjects[i] + "/obj-cbmc/cbmc.df." +  subject_names[i] + "_cbmc.result"
    cbmc_collection = get_testing_time(cbmc_data_file_path, mc_match_file_path, 'cbmc', subject_names[i], sys.argv[1])

    collection_size = len(cbmc_collection)

    if (not (len(cbmc_collection) == 0)):
        Q1, median, Q3 = np.percentile(np.asarray(cbmc_collection), [25, 50, 75])
        SIQR = (Q3-Q1)*1.0/2
        print '%s-%s, %d, %f, %f, %f, %f' % (subject_names[i], sys.argv[1], collection_size, Q1, median, Q3, SIQR)
        out = subject_names[i] + "-" + sys.argv[1] + ",cbmc," + str(collection_size) + "," + str(Q1) + "," + str(median) + "," + str(Q3) + "," + str(SIQR)
        fo.write(out)
        fo.write("\n")

    # plot box
    #plot_box(subject_names[i], cpa_collection, blast_collection, cbmc_collection)

    i = i+1

fo.close()
