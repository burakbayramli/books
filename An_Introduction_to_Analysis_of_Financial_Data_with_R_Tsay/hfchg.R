"hfchg" <- function(da){
# Compute the price changes in consecutive transactions
#
# int: time intervals in minutes
# da: data in the format: date, hour, minute, second, price, volume
#
if(!is.matrix(da))da=as.matrix(da)
istart=9*60*60+30*60
iend=16*60*60
nT=dim(da)[1]
# compute calendar time of trades in seconds from midnight.
caltime=da[,2]*60*60+da[,3]*60+da[,4]
# initializationn of variables
pchg=NULL
dur=NULL
size=NULL
## clean the data
idx=c(1:nT)[caltime < istart]
da1=da[-idx,]
caltime=caltime[-idx]
nT1=dim(da1)[1]
idx1=c(1:nT1)[caltime > iend]
da2=da1[-idx1,]
nT2=dim(da2)[1]
caltime=caltime[-idx1]
dtime=diff(caltime)
nT3=nT2-1
jdx=c(1:nT3)[dtime < 0]
ndays=length(jdx)
cat("number of trading days: ",ndays+1,"\n")
##
if(ndays==0){
pchg=diff(da2[,5])
dur=dtime
size=da2[2:nT2,6]
}
else{
jst=1
jdx=jdx+1
jdx=c(jdx,nT2)
for (ii in 1:(ndays+1)){
jend=jdx[ii]-1
cat("index ",c(jst,jend),"\n")
pchg=c(pchg,diff(da2[jst:jend,5]))
dur=c(dur,diff(caltime[jst:jend]))
size=c(size,da2[(jst+1):jend,6])
jst=jend+1
}
#
}




hfchg <- list(pchange=pchg,duration=dur,size=size)
}