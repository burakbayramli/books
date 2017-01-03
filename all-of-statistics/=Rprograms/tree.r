X <- scan("sa.data",skip=1,sep=",")
X <- matrix(X,ncol=11,byrow=T)
chd <- X[,11]
n <- length(chd)
X <- X[,-c(1,11)]
names <- c("sbp","tobacco","ldl","adiposity","famhist","typea","obesity","alcohol","age")
for(i in 1:9){
     assign(names[i],X[,i])
     }
famhist <- as.factor(famhist)
formula <- paste(names,sep="",collapse="+")
formula <- paste("chd ~ ",formula)
formula <- as.formula(formula)
chd     <- as.factor(chd)
d       <- data.frame(chd,sbp,tobacco,ldl,adiposity,famhist,typea,obesity,alcohol,age)

library(tree)
postscript("south.africa.tree.plot1.ps")
out <- tree(formula,data=d)
print(summary(out))
print(out)
plot(out,type="u",lwd=3)
text(out)
cv <- cv.tree(out,FUN=prune.tree,K=10,method="misclass")
size  <- cv$size
score <- cv$dev
dev.off()
postscript("south.africa.tree.plot2.ps")
plot(size,score,type="l",lwd=2,col=2)
k     <- size[score==min(score)]
k     <- k[1]
new   <- prune.tree(out,best=k,method="misclass")
print(summary(new))
print(new)
dev.off()
postscript("south.africa.tree.plot3.ps")
plot(new,lwd=3)
text(new)
dev.off()

