## Exercise 3.3 (b)
z.scores <- rep (NA, 100) 
for(k in 1:100){
  var1 <- rnorm (1000, 0.1)
  var2 <- rnorm (1000, 0.1)
  lm3.3 <- lm (var2 ~ var1)
  z.scores[k] <- coef(lm3.3)[2] / se.coef(lm3.3)[2] 
}
sum ( abs( z.scores ) > 2)