"boxtidwell.lm" <-
function (formula, data=list()) 
{
   mt <- terms(formula, data = data)
   mf <- match.call(expand.dots = FALSE)
   mf$singular.ok <- mf$model <- mf$method <- NULL
   mf$x <- mf$y <- NULL
   mf[[1]] <- as.name("model.frame")
   mf <- eval(mf, parent.frame())

   x <- model.matrix(mt, mf, contrasts)[,2]
   y <- model.response(mf, "numeric")
   y.x <- split(y, x)
     
   alpha <- 1       
   alpha.list <- alpha
   for (k in seq(0,3)){
   beta1 <- coef(lm(y~ I(x^alpha)))[2]
   beta2 <- coef(lm(y~ I(x^alpha) + I(x^alpha * log(x))))[3]
   alpha <- beta2/beta1 + alpha
   alpha.list <- c(alpha.list,alpha)
}
names(alpha.list) <- c("initial guess","alpha_1","alpha_2","alpha_3","final.estimate")
list(exponent = alpha.list)
    
}
