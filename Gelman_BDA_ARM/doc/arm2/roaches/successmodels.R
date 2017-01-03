success <- read.csv ("R2success.csv")
attach.all (success)
y <- success2

sqrt.roach1 <- sqrt (roach1)

M11 <- glm (y ~ sqrt.roach1 + treatment + senior, family=binomial(link="logit"))
display (M11)

M12 <- lmer (y ~ sqrt.roach1 + treatment + senior + (1 | building), family=binomial(link="logit"))
display (M12)
