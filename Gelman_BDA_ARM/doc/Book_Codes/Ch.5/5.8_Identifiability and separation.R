## Generating the variables

x <- rnorm(60, mean =1, sd = 2)
y <- ifelse(x<2,0,1)

## Fit the model

fit.0 <- glm (y ~ x, family=binomial(link="logit"))

## Plot

plot (x, y, xlab="x", ylab="y", xlim=c(-6,6), pch=20)
curve (invlogit (coef(fit.0)[1] + coef(fit.0)[2]*x), add=TRUE)





