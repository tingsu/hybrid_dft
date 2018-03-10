#!/usr/bin/env ruby

# This script finds an appropriate argument for --unwind and --depth for cbmc

$subject_index = ARGV[0].to_i
$best_covered_pair_cnt = ARGV[1].to_i
$best_unwind = ARGV[2].to_i
$best_depth = ARGV[3].to_i

def tune(to_be_tuned, current_unwind, current_depth, current_covered_pair_cnt) 

    while current_covered_pair_cnt >= $best_covered_pair_cnt do

        $best_covered_pair_cnt = current_covered_pair_cnt

        if to_be_tuned.eql?("unwind") then
            current_unwind = current_unwind - 2
        elsif to_be_tuned.eql?("depth") then
            current_depth = current_depth - 50
        end

        run_cbmc = "ruby run_dft_cbmc_for_tune.rb cbmc #{$subject_index} #{current_unwind} #{current_depth}"
        IO.popen(run_cbmc).each do |line|

            puts line
    
            if line.start_with?("statistics:") then

                i = line.index(":",11) 
                j = line.index(",")
                current_covered_pair_cnt = (line[(i+1)..j].strip()).to_i

            end
        end

        puts "the covered pairs cnt in this run: #{current_covered_pair_cnt}"

    end

    if to_be_tuned.eql?("unwind") then
        return current_unwind = current_unwind + 2
    elsif to_be_tuned.eql?("depth") then
        return current_depth = current_depth + 50
    end
end


$best_unwind = tune("unwind", $best_unwind, $best_depth, $best_covered_pair_cnt )
$best_depth = tune("depth", $best_unwind, $best_depth, $best_covered_pair_cnt)
puts "the best configuration: unwind: #{$best_unwind}, the best depth: #{$best_depth}"


