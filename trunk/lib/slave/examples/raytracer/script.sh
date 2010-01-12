#!/usr/bin/env ruby

require "open3"

def send(message)
  # cast the length of the message to four bytes representing
  # a network endian int
  header = [message.length].pack("N")
  $stdout.print(header, message)
  $stdout.flush
end

def receive()
  send("GET_DATA")
  # read four network endian bytes,
  # cast them to an array with an int, pop the int
  size = $stdin.read(4).unpack("N").pop
  case $stdin.read(5)
  when "SOME\n"
    data = $stdin.read(size - 5)
    # Extract the key and value from the given <<Key, "\n", Value>> binary
    i = data.index("\n")
    key = data[0,i]
    value = data[i+1,data.length]
    [key, value]
  when "NONE\n"
    nil
  end
end

def split(task_key, node_count)
  send("NEW_PID #{Process.pid}")
  key, data = receive()
  i = data.index("\n")
  size = data[0,i].to_i
  scene = data[i+1,data.length]
  split_count = node_count * 2
  split_size = size/split_count
  (0..(split_count - 1)).each do | i |
    split_id = ("%0" + split_count.to_s.length.to_s + "d") % i
    start = size - i*split_size
    stop = start - split_size
    send("NEW_MAP split#{i} scene\n#{size} #{start} #{stop}\n#{scene}")
  end
end

def map(task_key, pid_dir)
  send("NEW_PID #{Process.pid}")
  key, data = receive()
  i = data.index("\n")
  size, start, stop = data[0,i].split
  scene = data[i+1,data.length]
  scene_file = "#{pid_dir}/#{task_key}_scene"
  # comment out the correct one #\
  tracer = "./linux_tracer"       # }
  # tracer = "mac_tracer"      #/
  cl_code = "tracelines.cl"
  File.open(scene_file, 'w') do | file |
    file.puts(scene)
  end
  trace_command = "#{tracer} #{cl_code} #{start} #{stop} #{size} #{scene_file} #{pid_dir}"
  convert_command = "convert - -" # todo why does reduce not work with "png:-" here?
  result = `#{trace_command} | #{convert_command}`
  send("NEW_REDUCE reduce_all #{task_key}\n#{result}")
end

def reduce(task_key)
  send("NEW_PID #{Process.pid}")
  splits = ""
  map_key, image = receive()
  while (map_key) do
    splits += image
    map_key, image = receive()
  end
  splits_in, image_out, convert_errors = Open3.popen3("convert - -append png:-")
  splits_in.write(splits)
  splits_in.close
  result = ""
  while (piece = image_out.read 1024) do
    result += piece
  end
  send("NEW_FINAL_RESULT final_image.png\n#{result}")
end

def finalize(input, output)
  send("NEW_PID #{Process.pid}")
  send("NO_FINALIZING_PLZ")
end

command  = ARGV[0]
task_key = ARGV[1]
pid_dir  = ARGV[2]

send("LOG #{command} #{task_key} started.")

begin
  case command
  when "split"
    split(task_key, ARGV[3].to_i) # node count
  when "map"
    map(task_key, pid_dir)
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
