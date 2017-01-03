
#    IPSUR: Introduction to Probability and Statistics Using R
#    Copyright (C) 2014  G. Jay Kerns
#
#    Chapter: Probability
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
library(diagram)
library(prob)
library(RcmdrPlugin.IPSUR)

postscript(file="fig/prob-diagram.ps")
require(diagram)
par(mex = 0.2, cex = 0.5)
openplotmat(frame.plot=TRUE)
straightarrow(from = c(0.46,0.74), to = c(0.53,0.71), arr.pos = 1)
straightarrow(from = c(0.3,0.65), to = c(0.3,0.51), arr.pos = 1)
textellipse(mid = c(0.74,0.55), box.col = grey(0.95), 
  radx = 0.24, rady = 0.22, 
  lab = c(expression(bold(underline(DETERMINISTIC))), 
          expression(2*H[2]+O[2] %->% H[2]*O), "3 + 4 = 7"), cex = 2 )
textrect(mid = c(0.3, 0.75), radx = 0.15, rady = 0.1, 
  lab = c(expression(bold(Experiments))), cex = 2 )
textellipse(mid = c(0.29,0.25), box.col = grey(0.95), 
  radx = 0.27, rady = 0.22, lab = c(expression(bold(underline(RANDOM))), 
  "toss coin, roll die", "count ants on sidewalk", "measure rainfall" ), 
  cex = 2 )
dev.off()

S <- data.frame(lands = c("down","up","side"))
S

tosscoin(1)

tosscoin(3)

rolldie(1)

head(cards())

urnsamples(1:3, size = 2, replace = TRUE, ordered = TRUE)

urnsamples(1:3, size = 2, replace = FALSE, ordered = TRUE)

urnsamples(1:3, size = 2, replace = FALSE, ordered = FALSE)

urnsamples(1:3, size = 2, replace = TRUE, ordered = FALSE)

S <- tosscoin(2, makespace = TRUE) 
S[1:3, ]

S[c(2,4), ]

S <- cards()

subset(S, suit == "Heart")

subset(S, rank %in% 7:9)

subset(rolldie(3), X1+X2+X3 > 16)

x <- 1:10 
y <- 8:12 
y %in% x

isin(x,y)

x <- 1:10 
y <- c(3,3,7)

all(y %in% x)
isin(x,y)

isin(x, c(3,4,5), ordered = TRUE) 
isin(x, c(3,5,4), ordered = TRUE)

S <- rolldie(4) 
subset(S, isin(S, c(2,2,6), ordered = TRUE))

S <- cards() 
A <- subset(S, suit == "Heart") 
B <- subset(S, rank %in% 7:9)

union(A,B)

intersect(A,B)

setdiff(A,B)

setdiff(B,A)

outcomes <- rolldie(1) 
p <- rep(1/6, times = 6) 
probspace(outcomes, probs = p)

probspace(1:6, probs = p)

probspace(1:6)

rolldie(1, makespace = TRUE)

probspace(tosscoin(1), probs = c(0.70, 0.30))

S <- cards(makespace = TRUE) 
A <- subset(S, suit == "Heart") 
B <- subset(S, rank %in% 7:9)

Prob(A)

Prob(S, suit == "Heart")

nsamp(n=3, k=2, replace = TRUE, ordered = TRUE) 
nsamp(n=3, k=2, replace = FALSE, ordered = TRUE) 
nsamp(n=3, k=2, replace = FALSE, ordered = FALSE) 
nsamp(n=3, k=2, replace = TRUE, ordered = FALSE)

n <- c(11,7,31) 
k <- c(3,4,3) 
r <- c(FALSE,FALSE,TRUE)

x <- nsamp(n, k, rep = r, ord = TRUE)

prod(x)

(11*10*9)*(7*6*5*4)*31^3

prod(9:11)*prod(4:7)*31^3

prod(factorial(c(11,7))/factorial(c(8,3)))*31^3

postscript(file="fig/prob-birthday.ps")
g <- Vectorize(pbirthday.ipsur)
plot(1:50, g(1:50), xlab = "Number of people in room", ylab = "Prob(at least one match)")
remove(g)
dev.off()

library(RcmdrPlugin.IPSUR)
g <- Vectorize(pbirthday.ipsur)
plot(1:50, g(1:50), xlab = "Number of people in room", 
  ylab = "Prob(at least one match)" )
abline(h = 0.5)
abline(v = 23, lty = 2)
remove(g)

postscript(file="fig/prob-twodiceAB.ps")
A <- rolldie(2)
B <- subset(A, X1==X2)
C <- subset(A, X1+X2 > 7)
B$lab <- rep("X", dim(B)[1])
C$lab <- rep("O", dim(C)[1])
p <- ggplot(rbind(B, C), aes(x=X1, y=X2, label=lab))
p + geom_text(size = 15) + xlab("First roll") + ylab("Second roll")
dev.off()

S <- rolldie(2, makespace = TRUE)  # assumes ELM
head(S)                            #  first few rows

A <- subset(S, X1 == X2)
B <- subset(S, X1 + X2 >= 8)

Prob(A, given = B)
Prob(B, given = A)

Prob(S, X1==X2, given = (X1 + X2 >= 8) )
Prob(S, X1+X2 >= 8, given = (X1==X2) )

L <- cards()
M <- urnsamples(L, size = 2)
N <- probspace(M)
N[[1]][[1]];  N$probs[1]

Prob(N, all(rank == "A"))

L <- rep(c("red","green"), times = c(7,3))
M <- urnsamples(L, size = 3, replace = FALSE, ordered = TRUE)
N <- probspace(M)

Prob(N, isrep(N, "red", 3))

Prob(N, isrep(N, "red", 2))

Prob(N, isin(N, c("red","green","red"), ordered = TRUE))

Prob(N, isin(N, c("red","green","red")))

library(RcmdrPlugin.IPSUR)
data(RcmdrTestDrive)  
.Table <- xtabs( ~ smoking + gender, data = RcmdrTestDrive)
addmargins(.Table) # Table with marginal distributions

S <- tosscoin(10, makespace = TRUE)
A <- subset(S, isrep(S, vals = "T", nrep = 10))
1 - Prob(A)

iidspace(c("H","T"), ntrials = 3, probs = c(0.7, 0.3))

prior <- c(0.6, 0.3, 0.1)
like <- c(0.003, 0.007, 0.010)
post <- prior * like
post / sum(post)

newprior <- post
post <- newprior * like^7
post / sum(post)

fastpost <- prior * like^8
fastpost / sum(fastpost)

S <- rolldie(3, nsides = 4, makespace = TRUE) 
S <- addrv(S, U = X1-X2+X3)

head(S)

Prob(S, U > 6)

S <- addrv(S, FUN = max, invars = c("X1","X2","X3"), name = "V") 
S <- addrv(S, FUN = sum, invars = c("X1","X2","X3"), name = "W") 
head(S)

marginal(S, vars = "V")

marginal(S, vars = c("V", "W"))
