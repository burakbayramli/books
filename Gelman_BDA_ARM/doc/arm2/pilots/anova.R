anova.plot0 <- function (Summary, Source, Df, skip=rep(0,length(Source)),
    file="anova.ps", main="", height=4.5, width=5.5, x.max=NULL, signame="s.",
    sourcenames, plot.title){
  n.rows <- length(Source)
  postscript (file, horizontal=F, height=height, width=width)
  dns1 <- dimnames(Summary)[[1]]
  Infer.sd <- array (NA, c(n.rows,5))
  for (row in 1:n.rows)
    Infer.sd[row,] <- Summary[!is.na(match(dns1,paste(signame,Source[row],sep=""))),3:7]
  if (is.null(x.max)) x.max <- max(Infer.sd[,1:5])
  xaxis <- as.vector (pretty (c(0,x.max)))
  xaxis <- xaxis[xaxis<=1.1*x.max]
  plot (c(-1.3,1.1)*x.max, c(-n.rows-.5*sum(skip)-2,3),xlab="", ylab="", xaxs="i",
    yaxs="i", xaxt="n", yaxt="n", bty="n", type="n", main=main)
  text (x.max/2, 1.5, plot.title, cex=1)
  lines (c(0,max(xaxis)), rep(0,2))
  for (ax in xaxis){
    lines (rep(ax,2), c(-.1,.1), lwd=.5)
    text (ax, .6, ax, cex=.8)
  }
  ypos <- .5
    for (row in 1:n.rows){
      if (skip[row]==1) ypos <- ypos - 1.5
      else ypos <- ypos - 1
      text (-x.max/10, ypos, sourcenames[row], adj=1)
      lines (Infer.sd[row,c(1,5)], rep(ypos,2), lwd=.5)
      lines (Infer.sd[row,c(2,4)], rep(ypos,2), lwd=3)
      points (Infer.sd[row,3], ypos, pch=16, cex=.5)
    }
  lines (c(0,max(xaxis)), rep(ypos-.5,2))
  for (ax in xaxis){
    lines (rep(ax,2), ypos-.5+c(-.1,.1), lwd=.5)
    text (ax, ypos-1.1, ax, cex=.8)
  }
  lines (rep(0,2), c(0,ypos-1), lwd=.5, col=9)
dev.off()
}
