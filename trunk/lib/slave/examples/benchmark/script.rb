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

def send(message)
  header = [message.length].pack("N")
  $stdout.print(header, message)
  $stdout.flush
end

def receive()
  size = $stdin.read(4).unpack("N").pop
  case $stdin.read(5)
  when "SOME\n"
    $stdin.read(size - 5)
  when "NONE\n"
    nil
  end
end

def split(task_key, count)
  send("NEW_PID #{Process.pid}")
  send("GET_DATA")
  data = receive()
  lines = data.split("\n")
  case lines.shift
  when /split (\d+) (\d+) (\d+)/i
    split_time  = $1.to_i
    split_size  = $2.to_i
    split_count = $3.to_i
    before = Time.now
    map_input = []
    (1..split_count).each do |i|
      map_input[i] = lines.join("\n") << "\n"
      (1..split_size).each do |b|
        map_input[i] << 's'
      end
    end
    while (Time.now < before + split_time) do
    end
    (1..split_count).each do |i|
      send("NEW_MAP split#{i} key\n#{map_input[i]}")
    end
  else
    send("ERROR incorrect input line for split task")
  end
end

def map(task_key)
  send("NEW_PID #{Process.pid}")
  send("GET_DATA")
  data = receive()
  lines = data.split("\n")
  case lines.shift
  when /map (\d+) (\d+)/i
    map_time = $1.to_i
    map_size = $2.to_i
    before = Time.now
    reduce_input = lines[0,lines.length-1].join("\n") << "\n"
    (1..map_size).each do |b|
      reduce_input << 'm'
    end
    while (Time.now < before + map_time) do
    end
    send("NEW_REDUCE #{task_key} key\n#{reduce_input}")
  else
    puts "ERROR incorrect input line for map task"
  end
end

def reduce(task_key)
  send("NEW_PID #{Process.pid}")
  send("GET_DATA")
  data = receive()
  lines = data.split("\n")
  case lines.shift
  when /reduce (\d+) (\d+)/i
    reduce_time = $1.to_i
    reduce_size = $2.to_i
    before = Time.now
    finalize_input = lines[0,lines.length-1].join("\n") << "\n"
    (1..reduce_size).each do |b|
      finalize_input << 'r'
    end
    while (Time.now < before + reduce_time) do
    end
    send("NEW_RESULT #{task_key}\n#{finalize_input}")
  else
    send("ERROR incorrect input line for reduce task")
  end
end

def finalize(task_key)
  send("NEW_PID #{Process.pid}")
  send("GET_DATA")
  data = receive()
  lines = data.split("\n")
  case lines.shift
  when /finalize (\d+) (\d+)/i
    finalize_time = $1.to_i
    finalize_size = $2.to_i
    before = Time.now
    result = ""
    (1..finalize_size).each do |b|
      result << 'f'
    end
    while (Time.now < before + finalize_time) do
    end
    send("NEW_FINAL_RESULT #{task_key}\n#{result}")
  else
    send("ERROR incorrect input line for finalize task")
  end
end

# = Start working!

command = ARGV[0]
task_key = ARGV[1]

send("LOG I will #{command} \o/")

begin
  case command
  when "split"
    split(task_key, ARGV[3].to_i)
  when "map"
    map(task_key)
  when "reduce"
    reduce(task_key)
  when "finalize"
    finalize(task_key)
  else
    send("ERROR I can only split, map, reduce, and finalize!")
  end
rescue Exception => e
  send("ERROR #{e}\nBacktrace:\n#{e.backtrace}")
end
