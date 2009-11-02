
x = rnorm(100000)
y = rnorm(100000)
z = x + y

p <- scan("random.txt", list(0))
t = p[[1]]
g = c()
for(i in c(1:length(t))) 
{
  g = c(g,max(t[i],z))
}
l = c(mean(g))
l
