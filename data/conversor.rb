#!/usr/bin/ruby -w

file = File.open("saida3.dat")

while(line = file.gets) do
  if line =~ /^(.+),(.+),(.+),(.+),(.+)$/
    name = $1
    received_time = $3.to_i
    sent_time = $4.to_i
    computer = $5.to_i
    printf "%s\t%d\t%d\t%d\t%d\n", name,sent_time,received_time,(sent_time-received_time),computer

  end
end

file.close
