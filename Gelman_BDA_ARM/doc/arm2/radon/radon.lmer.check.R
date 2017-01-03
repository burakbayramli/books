# check what happens when adding xbar

radon.lmer.1 <- lmer (y ~ x + (1|county))
display (radon.lmer.1)
beta.hat (radon.lmer.1)$county[1:3,]


xbar <- tapply (x, county, mean)
xbar.full <- xbar[county]
radon.lmer.2 <- lmer (y ~ x + xbar.full + (1|county))
display (radon.lmer.2)
beta.hat (radon.lmer.2)$county[1:3,]
beta.hat.2 <- beta.hat(radon.lmer.2)$county
a.2 <- beta.hat.2[,1] + beta.hat.2[,3]*xbar

radon.lmer.3 <- lmer (y ~ x + u.full + (1|county))
display (radon.lmer.3)
radon.lmer.4 <- lmer (y ~ x + u.full + xbar.full + (1|county))
display (radon.lmer.4)
