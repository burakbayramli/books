#  Examples 17.9 and 17.10

library("fEcofin")
data("equityFunds")
options(digits=3)

#  Example 17.9
fa = factanal(equityFunds[,2:9],4,rotation="none")
fa
Beta = fa$loadings[,]
BetaBeta = Beta %*% t(Beta)
Sigma = diag(fa$unique)
BetaBeta + Sigma
diff = BetaBeta + Sigma - fa$corr
max(diff)
min(diff)
eig_diff = eigen(diff)
sort(eig_diff$values)
w = matrix(1/8,nrow =1,ncol=8)
w %*% BetaBeta %*% t(w)
w %*% fa$corr %*% t(w)

#  Example 17.10
fa_vari = factanal(equityFunds[,2:9],4,rotation="varimax")
fa_vari






