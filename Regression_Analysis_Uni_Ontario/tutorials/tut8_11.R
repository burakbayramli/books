## Regression Tutorial VIII

# ---------------- Q1 -------------------
# data input
firedata <-
structure(list(FireArea = c(11175.7, 834.8, 3745.9, 13.5, 30.3, 
7.2, 4, 3462.8, 215.4, 156, 34.6, 0.3, 6.4, 3751.3, 35949.7, 
31.9), Rain = c(698.8, 549.6, 562.3, 633, 647.9, 706.1, 730.7, 
508.4, 729.1, 710.2, 685.9, 717.2, 623, 546, 621.5, 701.7), TempMean = c(18.2670631999579, 
14.924373433584, 17.8271018455229, 15.6647368421053, 17.0458646616541, 
15.6619172932331, 15.7311387442966, 17.0984962406015, 17.9108604845447, 
15.8573308270677, 13.8637844611529, 13.4973684210526, 14.5624060150376, 
16.2854408749146, 15.3095655806182, 16.9991353383459), TempMax = c(30, 
28, 30.5, 31, 32.9, 30.6, 33, 33, 32, 30, 32, 28, 34, 30, 32, 
30), RHMean = c(56.4675456122825, 63.6385964912281, 56.9955570745044, 
52.7533834586466, 59.2512531328321, 52.3139097744361, 57.4353833301202, 
53.6115288220551, 58.5221386800334, 64.6597744360902, 58.6754385964912, 
64.6779448621554, 68.2639097744361, 61.8465481886535, 56.1708437761069, 
56.9714285714286), RHMin = c(22, 14, 14, 13, 25, 7, 15, 21, 18, 
19, 18, 22, 21, 20, 14, 22), WindMax = c(29, 31, 31, 28, 23, 
24, 27, 24, 26, 19, 25, 45, 20, 20, 28, 26), WindMean = c(13.0298385824702, 
14.1852130325815, 11.2050580997949, 8.75413533834586, 9.77380952380952, 
10.043984962406, 10.1388085598612, 9.78308270676692, 9.66081871345029, 
8.63533834586466, 7.61779448621554, 8.67418546365915, 8.96691729323308, 
8.83287764866712, 7.96365914786967, 8.23395989974937), DMC = c(18.9375, 
12.375, 22.5294117647059, 21.5294117647059, 17.1764705882353, 
16.6, 17.1875, 22.3333333333333, 16.8235294117647, 13.2941176470588, 
18.2352941176471, 12, 12.5625, 13.4117647058824, 18.625, 19.7058823529412
), FFMC = c(79.4375, 66.3125, 70.2352941176471, 71.2352941176471, 
69.2941176470588, 77.4, 68.25, 75.6, 68.5882352941177, 71.8235294117647, 
73.2352941176471, 69.5625, 69.31875, 71.9705882352941, 67.1875, 
68.5882352941177), BUI = c(28.5625, 18.25, 32.1176470588235, 
27.8823529411765, 26.1176470588235, 22.4, 23.3125, 31.2666666666667, 
22.5882352941176, 19.9411764705882, 22.5294117647059, 16.4375, 
18.5625, 21.2352941176471, 23.1875, 28.2352941176471), ISI = c(4.0875, 
3.2625, 3.87647058823529, 3.14705882352941, 1.67647058823529, 
3.70666666666667, 4.10625, 2.78, 2.68235294117647, 2.18823529411765, 
2.98823529411765, 2.58125, 1.9875, 2.31176470588235, 3.6875, 
2.74117647058824), DC = c(173.0625, 182.6875, 265.352941176471, 
129.176470588235, 189.882352941176, 107.333333333333, 151.875, 
197.866666666667, 144.529411764706, 152.176470588235, 94.6470588235294, 
90.75, 166.823529411765, 203, 115.266666666667, 167.588235294118
)), .Names = c("FireArea", "Rain", "TempMean", "TempMax", "RHMean", 
"RHMin", "WindMax", "WindMean", "DMC", "FFMC", "BUI", "ISI", 
"DC"), class = "data.frame", row.names = c(NA, -16L))

# build the linear regression model
Fires.lm <- lm(I(log(FireArea))~ Rain + TempMean + TempMax + RHMin + WindMax  
+  DMC + BUI + ISI, data=firedata)

## summary and cov matrix(unscaled)
summary(Fires.lm)
summary(Fires.lm)$cov.unscaled
anova(Fires.lm)

## to verify that lm.influence(***)$hat = t(x)(t(X)%*%X)^(-1)x
lm.influence(Fires.lm)$hat
X <- model.matrix(Fires.lm)
hat <- X%*%solve(t(X)%*%X)%*%t(X)
for(i in 1:dim(hat)[1])	print(hat[i,i])

## ----------- a ------------
## F test, F(2,7)
numerator <- 1.698+1.029
denominator <- (summary(Fires.lm)$sigma)^2
F <- numerator/2 / denominator
1-pf(F, 2, 7)


## ----------- b ------------
## F test F(4,7)
numerator <- 1.687+0.264+20.621+30.578
denominator <- (summary(Fires.lm)$sigma)^2
F <- numerator/4 / denominator
1-pf(F, 4, 7)

## another way
Fires.lm <- lm(I(log(FireArea))~ Rain + TempMean + TempMax + RHMin + WindMax  
+  DMC + BUI + ISI, data=firedata)
Fires.lm.redu <- lm(I(log(FireArea))~ Rain + TempMean + TempMax + RHMin, data=firedata)
anova(Fires.lm.redu,Fires.lm)

## ----------- c --------------
summary(Fires.lm)


## ----------- d --------------
T <- matrix(c(0,0,1,-2,-1,0,0,0,0,   0,0,0,0,0,0,1,-1,0),nrow=2, byrow = TRUE)
X <- model.matrix(Fires.lm)
XTX.inverse <- summary(Fires.lm)$cov.unscaled
beta.hat <- Fires.lm$coef
C <- T %*% XTX.inverse %*% t(T)
MSE <- (summary(Fires.lm)$sigma)^2
# F(2,7) test
F <- t(beta.hat) %*% t(T) %*% solve(C) %*% T %*% beta.hat / MSE
1-pf(F,2,7)

## -------------- e ------------
a <- anova(Fires.lm)
SSR <- sum(a[1:8,2])
SST <- sum(a[1:9,2])
SSR
SST
 

## ------------- f --------------
newdata <- data.frame(Rain=700,TempMean=18, TempMax=30, RHMin=22, WindMax=29, 
	DMC= 19, BUI=29, ISI=4.1)
predict(Fires.lm, newdata =newdata, interval="prediction")

max(lm.influence(Fires.lm)$hat)

XTX.inverse <- summary(Fires.lm)$cov.unscaled
x0 <- c(1,Rain=700,TempMean=18, TempMax=30, RHMin=22, WindMax=29, DMC= 19, BUI=29, ISI=4.1)
t(x0) %*% XTX.inverse %*% x0

if(t(x0) %*% XTX.inverse %*% x0 > max(lm.influence(Fires.lm)$hat)){
	cat("We are extrapolating\n")
}else{
	cat("We are not extrapolating\n")
}


## ------------- g --------------
newdata <- data.frame(Rain=520,TempMean=18, TempMax=30, RHMin=22, WindMax=29, 
	DMC= 19, BUI=29, ISI=4.1)
predict(Fires.lm, newdata =newdata, interval="prediction")

max(lm.influence(Fires.lm)$hat)

XTX.inverse <- summary(Fires.lm)$cov.unscaled
x0 <- c(1,Rain=520,TempMean=18, TempMax=30, RHMin=22, WindMax=29, DMC= 19, BUI=29, ISI=4.1)
t(x0) %*% XTX.inverse %*% x0

if(t(x0) %*% XTX.inverse %*% x0 > max(lm.influence(Fires.lm)$hat)){
	cat("We are extrapolating\n")
}else{
	cat("We are not extrapolating\n")
}




## ---------------------- Q2 ----------------------
library(DAAG)
cfseal
dim(cfseal)[1]

## ---------- a ----------
my.lm <- lm(log(weight)~log(heart)+log(stomach)+log(kidney),data=cfseal)


## to verify that lm.influence(***)$hat = t(x)(t(X)%*%X)^(-1)x
lm.influence(my.lm)$hat

#sigma <- summary(my.lm)$sigma
X <- model.matrix(my.lm)
hat <- X%*%solve(t(X)%*%X)%*%t(X)
for(i in 1:dim(hat)[1])	print(hat[i,i])

## ----------- b -----------
a <- anova(my.lm)
SSR <- sum(a[1:3,2])
MSR <- SSR/3
SSE <- a[4,2]
MSE <- SSE/26
F <- MSR/MSE
1-pf(F,3,26)


## ---------- c -----------
T <- matrix(c(0,1,-1,0,  0,0,1,-1),nrow=2, byrow = TRUE)
X <- model.matrix(my.lm)
XTX.inverse <- summary(my.lm)$cov.unscaled
beta.hat <- my.lm$coef
C <- T %*% XTX.inverse %*% t(T)
MSE <- (summary(my.lm)$sigma)^2
# F(2,26) test
F <- t(beta.hat) %*% t(T) %*% solve(C) %*% T %*% beta.hat / MSE
1-pf(F,2,26)

## -------- d ---------
newdata <- data.frame(heart=120, stomach=330, kidney=112)
newdata.log <- log(newdata)
predict(my.lm, newdata =newdata, interval="prediction")

## cretiria
max(lm.influence(my.lm)$hat)

XTX.inverse <- summary(my.lm)$cov.unscaled
x0 <- c(heart=120, stomach=330, kidney=112)
x0.log <- log(x0)
x0.log <- c(1,x0.log)
t(x0.log) %*% XTX.inverse %*% x0.log

if(t(x0.log) %*% XTX.inverse %*% x0.log > max(lm.influence(my.lm)$hat)){
	cat("We are extrapolating\n")
}else{
	cat("We are not extrapolating\n")
}



