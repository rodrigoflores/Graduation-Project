#!/usr/bin/ruby -w

file = File.open("db9.txt")

while(line = file.gets) do
  if line =~ /^(test1.+),(.+),(.+),(.+)$/
    name = $1
    received_time = $2.to_i
    sent_time = $3.to_i
    printf "%s\t%s\t%s\t%d\n", name,sent_time,received_time,-1*(received_time-sent_time)
  end
end

file.close
