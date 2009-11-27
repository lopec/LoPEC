#!/usr/bin/env ruby

require 'fileutils'

def split(input, output, split_count)
  puts("MY_PID #{Process.pid}")
  input_file = File.new(input, "r")
  side = input_file.gets.to_i
  scene_file = input_file.gets.chomp
  split_count = 2 * 1
  split_size = side/split_count
  (0..(split_count - 1)).each do | i |
    split_id = ("%0" + split_count.to_s.length.to_s + "d") % i
    File.open(output + "/split#{split_id}", "w") do | split_file |
      start = side - i*split_size
      stop = start - split_size
      split_file.puts("#{side} #{start} #{stop}")
      split_file.puts(scene_file)
      puts("NEW_SPLIT split#{split_id}")
    end
  end
end

def map(input, output, pid_dir)
  puts("MY_PID #{Process.pid}")
  split_id = input.split("/").last
  input_file = File.new(input, "r")
  dimensions = input_file.gets.split
  scene_file = input_file.gets.chomp
  side = dimensions[0].to_i
  start = dimensions[1].to_i
  stop = dimensions[2].to_i
  output_file = output + "/#{split_id}.png"
  prog_path = "/storage/test/programs/raytracer2"
  # comment out the correct one
  tracer = "#{prog_path}/linux_tracer"
  # tracer = "#{prog_path}/mac_tracer"
  cl_code = "#{prog_path}/tracelines.cl"
  system("#{tracer} #{cl_code} #{start} #{stop} #{side} #{scene_file} #{pid_dir} | convert - #{output_file}")
  puts("NEW_REDUCE_TASK #{split_id}.png")
end

def reduce(input, output)
  puts("MY_PID #{Process.pid}")
  split_id = input.split("/").last
  output_path = "#{output}/#{split_id}"
  system("cp #{input} #{output_path}")
  puts("NEW_REDUCE_RESULT #{output_path}")
end

def finalize(input, output)
  puts("MY_PID #{Process.pid}")
  output_path = output + "/final_image.png"
  system("convert #{input}/split* -append #{output_path}")
  puts("FINALIZING_DONE")
end

command = ARGV[0]
input   = ARGV[1]
output  = ARGV[2]
pid_dir = ARGV[3]

puts("LOG I will #{command} #{input} to #{output} now kthxbye.")

begin

case command
when "split"
  split(input, output, ARGV[4].to_i)
when "map"
  map(input, output, pid_dir)
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
