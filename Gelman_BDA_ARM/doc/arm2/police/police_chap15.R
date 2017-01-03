# Multilevel analysis of NYC police stops

# lmer() fits

M1 <- as.list (rep (NA, 12))
index <- 0
for (j in 1:3){
  for (k in 1:4){
    index <- index + 1
    ok <- precinct.category==j & crime==k
#    M1[[index]] <- lmer (y ~ dcjs + (1 | eth) + (1 | precinct),
#      family=quasipoisson(link="log"), subset=ok)
    M1[[index]] <- lmer (y ~ 1 + (1 | eth) + (1 | precinct),
                         offset=dcjs,
      family=quasipoisson(link="log"), subset=ok)
  }
}

display (M1[[1]])

beta.hat(M1[[1]])

beta.se(M1[[1]])

