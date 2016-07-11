# Chapter 2

# Simulation of basic inference and p-value
B = 10000
n = 30
alpha = 0.05
t.crit = qt(1 - alpha/2, n - 1)
coeff = t.crit/sqrt(n)

Res = vector(length=B)

for (i in 1:B) {
  x = rnorm(n)
  x.bar = mean(x)
  x.sd = sd(x)
  if (abs(x.bar) < coeff*x.sd) Res[i] = 0
  else                         Res[i] = 1  
}

sum(Res)/B


