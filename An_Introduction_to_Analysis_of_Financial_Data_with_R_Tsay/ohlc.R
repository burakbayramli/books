"ohlc_plot" <- function(datamat,title="Time",xl="",yl=""){
# Produce plot for daily stock prices: open, high, low, and close. (in order)
# datamat is the data matrix.
# This function was downloaded from Jussi Klemela's book web page and 
# modified. 
#
if(!is.matrix(datamat))da=as.matrix(datamat)

dendat<-datamat[,4]
low<-datamat[,3]
high<-datamat[,2]
open<-datamat[,1]
aika<-seq(1:length(dendat))

xmin<-1         
xmax<-length(dendat)
ymin<-min(low)
ymax<-max(high)

# frame
  plot(x="",y="",type="n",ylim=c(ymin,ymax),xlab=xl,ylab=yl,xlim=c(xmin,xmax),
       xaxt='n')
  minnu<-1
  vecpit<-minnu/3
  x0<-aika-vecpit
  y0<-open
  x1<-aika
  y1<-open
  segments(x0,y0,x1,y1)
  x0<-aika+vecpit
  y0<-dendat
  x1<-aika
  y1<-dendat
  segments(x0,y0,x1,y1)
  x0<-aika
  y0<-low
  x1<-aika
  y1<-high
  segments(x0,y0,x1,y1)
 title(main=title)
}