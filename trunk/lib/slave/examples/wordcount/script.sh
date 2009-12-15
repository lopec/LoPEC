#!/usr/bin/env ruby

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
  split_count = node_count * 2
  map_inputs = []
  (0..(split_count-1)).each do | i |
    map_inputs[i] = ""
  end
  key, data = receive()
  i = -1
  data.each_line do | line |
    if i >= split_count - 1 then
      i = 0
    else
      i += 1
    end
    map_inputs[i] << line
  end
  map_inputs.each_index do |i|
    send("NEW_MAP split#{i} #{task_key}\n#{map_inputs[i]}")
  end
end

def map(task_key)
  key, input = receive()
  word_count = Hash.new
  input.each_line do | line |
    line.split.each do | word |
      normalized = word.gsub(/[\W]/, '').downcase
      word_count[normalized] = word_count[normalized].to_i+1
    end
  end
  word_count.each_pair do | word, count |
    send("NEW_REDUCE #{word} #{task_key}\n#{count}")
  end
end

def reduce(word)
  word_count = 0
  map_key, count = receive()
  while (map_key) do
    send("LOG #{word} + #{count}")
    word_count += count.to_i
    map_key, count = receive()
  end
  send("NEW_RESULT #{word}\n#{word_count}")
end

def finalize(task_key)
  results = ""
  word, count = receive()
  while (word) do
    results << "#{word}: #{count}" << "\n"
    word, count = receive()
  end
  send("NEW_FINAL_RESULT #{task_key}\n#{results}")
end

command = ARGV[0]
task_key = ARGV[1]

send("LOG I will #{command}")

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
  send("LOG #{command} #{task_key} done")
rescue Exception => e
  send("ERROR #{e}\nBacktrace:\n#{e.backtrace}")
end
