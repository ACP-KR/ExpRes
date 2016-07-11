# Randomization Table for Three-way ANOVA without replication

R3w = function(l, m, n, Seed=100)
{
  set.seed(Seed)
  vFac1 = sort(rep(1:l, m*n))
  vFac2 = rep(sort(rep(1:l, m)), n)
  vFac3 = rep(1:l, m*n)
  vRan = sample(l*m*n)
  return(cbind(vFac1, vFac2, vFac3, vRan))
}
