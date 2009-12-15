#! /usr/bin/env ruby

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

def split(task_key, count)
  send("LOG <#{task_key}> splitting in #{count} splits.")
  key, value = receive()
  (1..count).each do |i|
    send("NEW_MAP map_#{i} map_#{i}\nsplit got (#{key} => #{value})")
  end
end

def map(task_key)
  send("LOG <#{task_key}> mapping.")
  key, value = receive()
  send("NEW_REDUCE #{task_key}_reduce #{task_key}_reduce\nmap got (#{key} => #{value})")
end

def reduce(task_key)
  send("LOG <#{task_key}> reducing.")
  key, value = receive()
  send("NEW_RESULT #{task_key}_finalize\nreduce got (#{key} => #{value})")
end

def finalize(task_key)
  send("LOG <#{task_key}> finalizing.")
  key, value = receive()
  send("NEW_FINAL_RESULT #{task_key}_result\nfinalize got (#{key} => #{value})")
end

# = Start working!

command = ARGV[0]
task_key = ARGV[1]

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
