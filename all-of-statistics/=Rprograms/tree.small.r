X   = scan("sa.data",skip=1,sep=",")
X   = matrix(X,ncol=11,byrow=T)
chd = X[,11]
n   = length(chd)
X   = X[,-c(1,11)]
names = c("sbp","tobacco","ldl","adiposity","famhist","typea","obesity","alcohol","age")
for(i in 1:9){
     assign(names[i],X[,i])
     }
famhist = as.factor(famhist)
formula = paste(names,sep="",collapse="+")
formula = "chd ~ age + tobacco"
formula = as.formula(formula)
chd     = as.factor(chd)
d       = data.frame(chd,tobacco,age)

library(tree)
postscript("south.africa.tree.small.plot1.ps")
out = tree(formula,data=d)
print(summary(out))
print(out)
plot(out,type="u",lwd=3)
text(out)
dev.off()
postscript("south.africa.tree.small.plot2.ps")
partition.tree(out,lwd=3)
dev.off()



