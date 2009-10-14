#!/usr/bin/env ruby

require 'fileutils'

def split(input, output)
  split_count = 0
  File.new(input, "r").each_line do | line |
    split_name = "split#{split_count}"
    File.open(output + "/" + split_name, "w") do | split_file |
      split_file.puts("#{line}")
    end
    split_count += 1
    puts("NEW_SPLIT #{split_name}")
  end
end

def map(input, output)
  split_id = input.split("/").last
  word_count = Hash.new
  File.new(input, "r").each_line do | line |
    line.split.each do | word |
      word_count[word] = word_count[word].to_i+1
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

begin
  puts("LOG I will #{command} #{input} to #{output} now plz.")
  case command
  when "split"
    split(input, output)
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
ensure
  $stdout.flush
end
