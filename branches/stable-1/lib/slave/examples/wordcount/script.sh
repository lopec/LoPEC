#!/usr/bin/env ruby

require 'fileutils'

def split(input, output, split_count)
  split_files = []
  (0..(split_count.to_i-1)).each do | i |
    split_files[i] = File.open(output + "/split#{i}", "w")
  end
  i = -1
  File.new(input, "r").each_line do | line |
    if i >= split_count.to_i - 1 then
      i = 0
    else
      i += 1
    end
    split_files[i].puts("#{line}")
  end
  split_files.each_index do | i |
    split_files[i].close
    puts("NEW_SPLIT split#{i}")
  end
end

def map(input, output)
  split_id = input.split("/").last
  word_count = Hash.new
  File.new(input, "r").each_line do | line |
    line.split.each do | word |
      normalized = word.gsub(/[\W]/, '').downcase
      word_count[normalized] = word_count[normalized].to_i+1
    end
  end
  word_count.each_pair do | key, value |
    word_path = output + "/" + key
    FileUtils.mkdir_p(word_path)
    File.open(word_path + "/" + split_id, "a") do | word_file |
      word_file.puts("#{value}")
    end
    puts("NEW_REDUCE_TASK #{key}")
  end
end

def reduce(input, output)
  word = input.split("/").last
  word_count = 0
  Dir.foreach(input) do | task |
    if task != "." and task != ".." then
      File.new(input + "/" + task, "r").each_line do | count_line |
        word_count += count_line.to_i
      end
    end
  end
  File.open(output + "/" + word, "w") do | output_file |
    output_file.puts("#{word_count}")
    puts("NEW_REDUCE_RESULT #{word}")
  end
end

def finalize(input, output)
  File.open(output + "/word_count", "w") do | output_file |
    Dir.foreach(input) do | word_file |
      if word_file != "." and word_file != ".." then
        File.open(input + "/" + word_file, "r") do | input_file |
          count = input_file.gets
          output_file.puts("#{word_file}: #{count}")
        end
      end
    end
  end
  puts("FINALIZING_DONE")
end

command = ARGV[0]
input   = ARGV[1]
output  = ARGV[2]

puts("LOG I will #{command} #{input} to #{output} now kthxbye.")

begin

case command
when "split"
  split(input, output, ARGV[3])
when "map"
  map(input, output)
when "reduce"
  reduce(input, output)
when "finalize"
  finalize(input, output)
else
  puts("ERROR I can only split, map, reduce, and finalize!")
end

rescue
  puts("ERROR #{$!}")
end
