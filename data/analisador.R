table = read.table("saida2.dat")

mean(table[[4]])
sd(table[[4]])
min(table[[2]]) - max(table[[3]])

pdf(file="hist_geral.pdf")
hist(table[[4]], xlab="Tempo de execução (s)", ylab = "Frequência", main="Histograma para o tempo de execução das tarefas")
dev.off()

(300*mean(table[[4]]))

l = c()
z = table[[4]]
y = table[[5]]
k = table[[3]]
m = table[[2]]
a = 0
for(i in 1:300)
{
  if(y[i] == 26)
  {
    a = a +1
    l = c(l,TRUE)
  }
  else
  {
    l = c(l,FALSE)
  }
}
a

mean(z[l])
sd(z[l])
300.00*mean(z[l])
min(m[l]) - max(k[l])
pdf(file="hist1.pdf")
hist(z[l], ,xlab="Tempo de execução (s)", main="Histograma dos tempos de execução das tarefas na máquina 1", ylab="Frequência")
dev.off()

mean(z[!l])
sd(z[!l])
300*mean(z[!l])
min(m[!l]) - max(k[!l])
pdf(file="hist2.pdf")
hist(z[!l], ,xlab="Tempo de execução (s)", main="Histograma dos tempos de execução das tarefas na máquina 2", ylab="Frequência")
dev.off()
