# the function to make a graph out of the regression coeffs and se's

regression.table <- function (name, est, se, file, bottom=FALSE, gaps=rep(0,length(name))){
  J <- length(name)
  name.range <- .6
  x.range <- range (est+2*se, est-2*se)
  A <- -x.range[1]/(x.range[2]-x.range[1])
  B <- 1/(x.range[2]-x.range[1])
  height <- .35*J
  width <- 3*(name.range+1)
  
  postscript (file, horizontal=F, height=height, width=width)
  par (mar=c(0,0,0,0))
  plot (c(-name.range,1), c(3,-J-2-sum(gaps)), bty="n", xlab="", ylab="",
        xaxt="n", yaxt="n", xaxs="i", yaxs="i", type="n")
  text (-name.range, 2, "Coefficient", adj=0, cex=1)
  text (.5, 2, "Estimate", adj=.5, cex=1)
  lines (c(0,1), c(0,0))
  lines (c(A,A), c(0,-J-1-sum(gaps)), lty=2, lwd=.5)
  ax <- pretty (x.range)
  ax <- ax[(A+B*ax)>0 & (A+B*ax)<1]
  segments (A + B*ax, -.1, A + B*ax, .1, lwd=.5)
  text (A + B*ax, .7, ax, cex=1)
  text (-name.range, -(1:J)-cumsum(gaps), name, adj=0, cex=1)
  points (A + B*est, -(1:J)-cumsum(gaps), pch=20, cex=.7)
  segments (A + B*(est-se), -(1:J)-cumsum(gaps), A + B*(est+se), -(1:J)-cumsum(gaps), lwd=1.5)
  segments (A + B*(est-2*se), -(1:J)-cumsum(gaps), A + B*(est+2*se), -(1:J)-cumsum(gaps), lwd=.5)
  if (bottom){
    lines (c(0,1), c(-J-1,-J-1)-sum(gaps))
    segments (A + B*ax, -J-1-.1-sum(gaps), A + B*ax, -J-1+.1-sum(gaps), lwd=.5)
    text (A + B*ax, -J-1-.7-sum(gaps), ax, cex=1)
  } 
  graphics.off()
}

# a couple of examples

names <- c("female", "nonwhite", "age < 30", "age > 65", "married", "college educated", "employed", "income < $20,000", "income > $80,000")
ests <- c(-.13,.52,.19,-.14,-.19,-.12,-.11,-.02,-.15)
ses <- c(.04,.06,.05,.07,.07,.05,.05,.06,.07)
regression.table (names, ests, ses, "resid_table.ps")
ests <- c(-.11,.06,-.02,-.14,.04,.11,.13,-.18,.18)
ses <- c(.03,.04,.04,.05,.05,.03,.04,.05,.05)
regression.table (names, ests, ses, "alpha_table.ps")

# display 2 regressions

regression.2tables <- function (name, est1, est2, se1, se2, label1, label2, file, bottom=FALSE){
  J <- length(name)
  name.range <- .6
  x.range <- range (est1+2*se1, est1-2*se1, est2+2*se2, est1-2*se2)
  A <- -x.range[1]/(x.range[2]-x.range[1])
  B <- 1/(x.range[2]-x.range[1])
  height <- .6*J
  width <- 8*(name.range+1)
  gap <- .4
  
  postscript (file, horizontal=F, height=height, width=width)
  par (mar=c(0,0,0,0))
  plot (c(-name.range,2+gap), c(3,-J-2), bty="n", xlab="", ylab="",
        xaxt="n", yaxt="n", xaxs="i", yaxs="i", type="n")
  text (-name.range, 2, "Subpopulation", adj=0, cex=1)
  text (.5, 2, label1, adj=.5, cex=1)
  text (1+gap+.5, 2, label2, adj=.5, cex=1)
  lines (c(0,1), c(0,0))
  lines (1+gap+c(0,1), c(0,0))
  lines (c(A,A), c(0,-J-1), lty=2, lwd=.5)
  lines (1+gap+c(A,A), c(0,-J-1), lty=2, lwd=.5)
  ax <- pretty (x.range)
  ax <- ax[(A+B*ax)>0 & (A+B*ax)<1]
  segments (A + B*ax, -.1, A + B*ax, .1, lwd=.5)
  segments (1+gap+A + B*ax, -.1, 1+gap+A + B*ax, .1, lwd=.5)
  text (A + B*ax, .7, ax, cex=1)
  text (1+gap+A + B*ax, .7, ax, cex=1)
  text (-name.range, -(1:J), name, adj=0, cex=1)
  points (A + B*est1, -(1:J), pch=20, cex=1)
  points (1+gap+A + B*est2, -(1:J), pch=20, cex=1)
  segments (A + B*(est1-se1), -(1:J), A + B*(est1+se1), -(1:J), lwd=3)
  segments (1+gap+A + B*(est2-se2), -(1:J), 1+gap+A + B*(est2+se2), -(1:J), lwd=3)
  segments (A + B*(est1-2*se1), -(1:J), A + B*(est1+2*se1), -(1:J), lwd=.5)
  segments (1+gap+A + B*(est2-2*se2), -(1:J), 1+gap+A + B*(est2+2*se2), -(1:J), lwd=.5)
  if (bottom){
    lines (c(0,1), c(-J-1,-J-1))
    lines (1+gap+c(0,1), c(-J-1,-J-1))
    segments (A + B*ax, -J-1-.1, A + B*ax, -J-1+.1, lwd=.5)
    segments (1+gap+A + B*ax, -J-1-.1, 1+gap+A + B*ax, -J-1+.1, lwd=.5)
    text (A + B*ax, -J-1-.7, ax, cex=1)
    text (1+gap+A + B*ax, -J-1-.7, ax, cex=1)
  } 
  graphics.off()
}

regression.2tablesA <- function (name, est1, se1, label1, file, bottom=FALSE){
  J <- length(name)
  name.range <- .6
  x.range <- range (est1+2*se1, est1-2*se1)
  A <- -x.range[1]/(x.range[2]-x.range[1])
  B <- 1/(x.range[2]-x.range[1])
  height <- .6*J
  width <- 8*(name.range+1)
  gap <- .4
  
  postscript (file, horizontal=F, height=height, width=width)
  par (mar=c(0,0,0,0))
  plot (c(-name.range,2+gap), c(3,-J-2), bty="n", xlab="", ylab="",
        xaxt="n", yaxt="n", xaxs="i", yaxs="i", type="n")
  text (-name.range, 2, "Subpopulation", adj=0, cex=1)
  text (.5, 2, label1, adj=.5, cex=1)
  lines (c(0,1), c(0,0))
  lines (c(A,A), c(0,-J-1), lty=2, lwd=.5)
  ax <- pretty (x.range)
  ax <- ax[(A+B*ax)>0 & (A+B*ax)<1]
  segments (A + B*ax, -.1, A + B*ax, .1, lwd=.5)
  text (A + B*ax, .7, ax, cex=1)
  text (-name.range, -(1:J), name, adj=0, cex=1)
  points (A + B*est1, -(1:J), pch=20, cex=1)
  segments (A + B*(est1-se1), -(1:J), A + B*(est1+se1), -(1:J), lwd=3)
  segments (A + B*(est1-2*se1), -(1:J), A + B*(est1+2*se1), -(1:J), lwd=.5)
  if (bottom){
    lines (c(0,1), c(-J-1,-J-1))
    segments (A + B*ax, -J-1-.1, A + B*ax, -J-1+.1, lwd=.5)
    text (A + B*ax, -J-1-.7, ax, cex=1)
  } 
  graphics.off()
}

