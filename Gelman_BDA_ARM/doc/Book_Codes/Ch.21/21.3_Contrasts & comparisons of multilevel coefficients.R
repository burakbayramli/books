## The finite-population group-level coefficient

attach.bugs(AP.fit)
finite.slope <- rep (NA, n.sims)
for (s in 1:n.sims){
  finite.pop <- lm (a[s,] ~ u)
  finite.slope[s] <- coef(finite.pop)["u"]
}
quantile (finite.pop, c(.025, .975))


