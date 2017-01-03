
#    IPSUR: Introduction to Probability and Statistics Using R
#    Copyright (C) 2014  G. Jay Kerns
#
#    Chapter: Multivariate Distributions
#
#    This file is part of IPSUR.
#
#    IPSUR is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    IPSUR is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with IPSUR.  If not, see <http://www.gnu.org/licenses/>.

# This chapter's package dependencies
library(prob)
library(ggplot2)
library(mvtnorm)
library(lattice)
library(grid)

postscript(file="fig/multdist-max-and-sum-two-dice.ps")
A <- rolldie(2, makespace = TRUE)
A <- addrv(A, max, name = "U")
A <- addrv(A, sum, name = "V", invars = c("X1", "X2"))
p1 <- ggplot(A, aes(x=X1, y=X2, label=U))
p1 <- p1 + geom_text(size = 6) + xlab("X1 = First roll") + 
      ylab("X2 = Second roll") + labs(title = "U = max(X1,X2)")
p2 <- ggplot(A, aes(x=X1, y=X2, label=V))
p2 <- p2 + geom_text(size = 6) + xlab("X1 = First roll") + 
      ylab("") + labs(title = "V = X1 + X2")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(1, 2))
dev.off()

postscript(file="fig/multdist-max-sum-two-dice-joint.ps")
labs <- with(A, paste("(", U, ",", V, ")", sep = ""))
p3 <- ggplot(A, aes(x = X1, y = X2, label = labs))
p3 + geom_text(size = 6) + xlab("First roll") + ylab("Second roll") + 
  labs(title = "Joint distribution of (U,V) pairs")
dev.off()

S <- rolldie(2, makespace = TRUE)
S <- addrv(S, FUN = max, invars = c("X1","X2"), name = "U")
S <- addrv(S, FUN = sum, invars = c("X1","X2"), name = "V")
head(S)

UV <- marginal(S, vars = c("U", "V"))
head(UV)

xtabs(round(probs,3) ~ U + V, data = UV)

marginal(UV, vars = "U")
head(marginal(UV, vars = "V"))

temp <- xtabs(probs ~ U + V, data = UV)
rowSums(temp)
colSums(temp)

Eu <- sum(S$U*S$probs)
Ev <- sum(S$V*S$probs)
Euv <- sum(S$U*S$V*S$probs)
Euv - Eu * Ev

x <- y <- seq(from = -3, to = 3, length.out = 30)
f <- function(x,y) dmvnorm(cbind(x,y), mean = c(0,0), sigma = diag(2))
z <- outer(x, y, FUN = f)
persp(x, y, z, theta = -30, phi = 30, ticktype = "detailed")

postscript(file="fig/multdist-mvnorm-pdf.ps")
x <- y <- seq(from = -3, to = 3, length.out = 30)
f <- function(x,y) dmvnorm(cbind(x,y), mean = c(0,0), sigma = diag(2))
z <- outer(x, y, FUN = f)
persp(x, y, z, theta = -30, phi = 30, ticktype = "detailed")
dev.off()

tmp <- t(xsimplex(3, 6))
p <- apply(tmp, MARGIN = 1, FUN = dmultinom, prob = c(36,27,37))
S <- probspace(tmp, probs = p)
ProbTable <- xtabs(probs ~ X1 + X2, data = S)
round(ProbTable, 3)

cloud(probs ~ X1 + X2, data = S, type = c("p","h"), lwd = 2, 
            pch = 16, cex = 1.5, screen = list(z = 15, x = -70))

postscript(file="fig/multdist-multinom-pmf2.ps")
print(cloud(probs ~ X1 + X2, data = S, type = c("p","h"), lwd = 2, 
            pch = 16, cex = 1.5), screen = list(z = 15, x = -70))
dev.off()
