######################## © CSIRO Australia 2005 ###############################
# Session 15:  Tree-based Models:  Classification Trees                       #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################


#######################
# Classification Trees

# Fitting the model
require(rpart)
set.seed(123)
iris.rp <- rpart(Species ~ ., method="class", data=iris,
        control=rpart.control(minsplit=4,cp=0.00001))
summary(iris.rp)

# Plotting the tree
plot(iris.rp)
text(iris.rp)

# Recursive partitioning up close
require(lattice)
trellis.par.set(col.whitebg())
xyplot(Petal.Width~Petal.Length,iris,groups=Species,pch=16,
    col=c("red","green","blue"),
    panel=function(x,y,groups,...){
     panel.superpose(x,y,groups,...)
     panel.abline(v=2.45,lty=2)
     panel.segments(2.45,1.75,max(x)*2,1.75,lty=2)
     panel.segments(4.95,min(y)*-2,4.95,1.75,lty=2)
     panel.segments(2.45,1.65,4.95,1.65,lty=2)
     panel.segments(4.95,1.55,max(x)*2,1.55,lty=2)
   },
  key=list(columns=3,col=c("red","green","blue"),
    text=list(c("setosa","versicolor","virginica"))))

# Complexity Table
printcp(iris.rp)
plotcp(iris.rp)

 # Plotting CP Table
 # Plot the resubstitution error
 with(iris.rp,plot(cptable[,3],xlab="Tree Number",
     ylab="Resubstitution Error (R)",type="b"))

 # Plotting the cross-validated error
 with(iris.rp,plot(cptable[,4],xlab="Tree Number",
     ylab="Cross-Validated Error (R(cv))",type="b"))

 # Illustration of the 1SE Rule
 plotcp(iris.rp)
 with(iris.rp, {
   lines(cptable[,2]+1,cptable[,3],type="b",col="red")
  legend(locator(1),c("Resub. Error","CV Error","min(CV Error)+1SE"),
    lty=c(1,1,2),col=c("red","black","black"),bty="n")
  })
    
# Pruning Tree
iris.prune <- prune(iris.rp,cp=0.1)
summary(iris.prune)
plot(iris.prune)
text(iris.prune)

# Partitioned Data
require(lattice)
trellis.par.set(theme=col.whitebg())
xyplot(Petal.Width~Petal.Length,iris,groups=Species,pch=16,
  col=c("red","green","blue"),main="Partitioned Plot",
    panel=function(x,y,groups,...){
     panel.superpose(x,y,groups,...)
     panel.abline(v=2.45,lty=2)
     panel.segments(2.45,1.75,max(x)*2,1.75,lty=2)
   },
  key=list(columns=3,col=c("red","green","blue"),
    text=list(c("setosa","versicolor","virginica"))))

# Plotting Options
# Version A (Default)
plot(iris.prune,main="Version A")
text(iris.prune)
# Version B
plot(iris.prune,uniform=T,branch=0.1,main="Version B")
text(iris.prune,pretty=1,use.n=T)
# Version C
plot(iris.prune,uniform=T,branch=0.1,margin=0.1,main="Version C")
text(iris.prune,pretty=1,all=T,use.n=T,fancy=T)
# Postscript Version
post(iris.prune,title="Postscript Version",filename="iris.ps")

# Predictions
iris.pred <- predict(iris.prune,type="class")
table(iris.pred,iris$Species)

