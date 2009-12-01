#! /usr/bin/env ruby

require 'fileutils'

# = Benchmark script
#
# This script can be configured to behave in different ways for
# benchmarking purposes.
#
# You can customize the script by using an input file on the following format:
#
# split    $SPLITTIME $SPLITSIZE $SPLITCOUNT
# map      $MAPTIME $MAPSIZE
# reduce   $REDUCETIME $REDUCESIZE
# finalize $FINALIZETIME $FINALIZESIZE

# = Helpers

def split(input, output, count)
  puts "MY_PID #{Process.pid}"
  File.open(input, 'r') do |input_file|
    case input_file.gets.strip
    when /split (\d+) (\d+) (\d+)/i
      split_time  = $1.to_i
      split_size  = $2.to_i
      split_count = $3.to_i
      before = Time.now
      begin
        map_line = input_file.gets
        reduce_line = input_file.gets
        finalize_line = input_file.gets
      rescue
        puts "ERROR incorrect map/reduce/finalize lines in split input"
      end
      (1..split_count).each do |i|
        File.open("#{output}/split#{i}", 'w') do |output_file|
          output_file.puts map_line
          output_file.puts reduce_line
          output_file.puts finalize_line
          (1..split_size).each do |b|
            output_file.write('s')
          end
        end
      end
      while (Time.now < before + split_time) do
      end
      (1..split_count).each do |i|
        puts "NEW_SPLIT #{output}/split#{i}"
      end
    else
      puts "ERROR incorrect input line for split task"
    end
  end
end

def map(input, output)
  puts "MY_PID #{Process.pid}"
  File.open(input, 'r') do |input_file|
    case input_file.gets.strip
    when /map (\d+) (\d+)/i
      map_time = $1.to_i
      map_size = $2.to_i
      before = Time.now
      begin
        reduce_line = input_file.gets
        finalize_line = input_file.gets
      rescue
        puts "ERROR incorrect reduce/finalize lines in map input"
      end
      id = input.split('/').last
      File.open("#{output}/#{id}", 'w') do |output_file|
        output_file.puts reduce_line
        output_file.puts finalize_line
        (1..map_size).each do |b|
          output_file.write('m')
        end
      end
      while (Time.now < before + map_time) do
      end
      puts "NEW_REDUCE_TASK #{output}/#{id}"
    else
      puts "ERROR incorrect input line for map task"
    end
  end
end

def reduce(input, output)
  puts "MY_PID #{Process.pid}"
  File.open(input, 'r') do |input_file|
    case input_file.gets.strip
    when /reduce (\d+) (\d+)/i
      reduce_time = $1.to_i
      reduce_size = $2.to_i
      before = Time.now
      begin
        finalize_line = input_file.gets
      rescue
        puts "ERROR incorrect finalize line in reduce input"
      end
      id = input.split('/').last
      File.open("#{output}/#{id}", 'w') do |output_file|
        output_file.puts finalize_line
        (1..reduce_size).each do |b|
          output_file.write('r')
        end
      end
      while (Time.now < before + reduce_time) do
      end
      puts("NEW_REDUCE_RESULT #{output}")
    else
      puts "ERROR incorrect input line for reduce task"
    end
  end
end

def finalize(input, output)
  puts "MY_PID #{Process.pid}"
  File.open("#{input}/split1", 'r') do |input_file|
    case input_file.gets.strip
    when /finalize (\d+) (\d+)/i
      finalize_time = $1
      finalize_size = $2
      before = Time.now
      File.open("#{output}/finalized", "w") do | output_file |
        output_file.puts("Nothing to see here, move along.")
      end
      while (Time.now < before + finalize_time) do
      end
      puts("FINALIZING_DONE")
    else
      puts "ERROR incorrect input line for map task"
    end
  end
end

# = Start working!

command = ARGV[0]
input   = ARGV[1]
output  = ARGV[2]

puts "LOG I will #{command} from #{input} to #{output}"

begin
  case command
  when "split"
    split(input, output, ARGV[4].to_i)
  when "map"
    map(input, output)
  when "reduce"
    reduce(input, output)
  when "finalize"
    finalize(input, output)
  else
    puts("ERROR I can only split, map, reduce, and finalize!")
  end
rescue Exception => e
  puts("ERROR #{e}\nBacktrace:\n#{e.backtrace}")
end
