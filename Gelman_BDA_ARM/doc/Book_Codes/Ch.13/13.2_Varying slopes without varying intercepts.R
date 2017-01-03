## Varying slope without varying intercepts in R

 # with treatment as a predictor
fit.1 <- lmer (y ~ T + (T - 1 | group)

 # with x as an individual-level predictor
fit.2 <- lmer (y ~ x + T + (T + x:T - 1 | group)



