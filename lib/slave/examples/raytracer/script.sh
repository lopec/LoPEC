#!/usr/bin/env ruby

require 'fileutils'

def split(input, output, split_count)
  side = File.new(input, "r").gets.to_i
  split_count = (side*side) / (1024*1024)
  split_size = side/split_count
  (0..(split_count - 1)).each do | i |
    split_id = ("%0" + split_count.to_s.length.to_s + "d") % i
    File.open(output + "/split#{split_id}", "w") do | split_file |
      start = side - i*split_size
      stop = start - split_size
      split_file.puts("#{side} #{start} #{stop}")
      puts("NEW_SPLIT split#{split_id}")
    end
  end
  File.open(output + "/header", "w") do | header_file |
    header_file.puts("P2 #{side} #{side} 255\n")
      puts("NEW_SPLIT header")
  end
end

def map(input, output)
  split_id = input.split("/").last
  case split_id
  when "header"
    system("cp #{input} #{output}/header")
  else
    scene_spec = File.new(input, "r").gets.split
    side = scene_spec[0].to_i
    start = scene_spec[1].to_i
    stop = scene_spec[2].to_i
    output_file = output + "/#{split_id}"
    prog_path = "/storage/test/programs/raytracer"
    tracer = "#{prog_path}/oclRaytrace"
    cl_code = "#{prog_path}/traceLines.cl"
    system("#{tracer} #{cl_code} #{start} #{stop} #{side} > #{output_file}")
  end
  puts("NEW_REDUCE_TASK #{split_id}")
end

def reduce(input, output)
  split_id = input.split("/").last
  system("cp #{input} #{output}/#{split_id}")
  unless (File.directory? output) # check to see that the results directory does not exist
    puts("NEW_REDUCE_RESULT #{output}")
end

def finalize(input, output)
  output_path = output + "/final_image.pgm"
  system("cat #{input}/header > #{output_path}")
  system("cat #{input}/split* >> #{output_path}")
  puts("FINALIZING_DONE")
end

command = ARGV[0]
input   = ARGV[1]
output  = ARGV[2]

puts("LOG I will #{command} #{input} to #{output} now kthxbye.")

begin

case command
when "split"
  split(input, output, ARGV[3].to_i)
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
