H.6 <- matrix(rbind(diag(3), c(0, 0, 0), c(0, 0, 0)), nrow=5, ncol=3)
H6 <- summary(bh6lrtest(z=H1, H=H.6, r=2, r1=1))
