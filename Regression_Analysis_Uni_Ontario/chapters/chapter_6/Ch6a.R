"lof.lm" <-
function (lm.obj, approx = FALSE, call.plot=TRUE) 
{
   Xmatrix <- model.matrix(lm.obj)
   p <- dim(Xmatrix)[2]
   if (p==2) {x <- Xmatrix[,-1]}
       else {x <- Xmatrix[,1]}
   y <- model.response(lm.obj$model, "numeric")
   if (call.plot) {plot(y~x)
                   abline(lm.obj)}
   if (approx) {
      cat("The following results are only approximate!!! \n")
      x.sort <- sort(x)
      y <- y[order(x)] 
      n <- length(x)
      new.indices <- rep(seq(1,(n+1)/2),rep(2,(n+1)/2))[1:n]
      x <- rep(sapply(split(x.sort,new.indices),mean),rep(2,(n+1)/2))[1:n]
      lm.obj <- lm(y~x)
      if (call.plot) {points(x,y,pch=16,col=2)
                      abline(lm.obj,col=2)
                      title(sub="red denotes approximation")}
    }  
   y.x <- split(y, x)
   if (call.plot) {group.means <- sapply(y.x, mean)
                   points(sort(unique(x)),group.means, pch=16, col=4)}
   dfPE <- sum(sapply(y.x, length))-length(y.x)
   if (dfPE == 0) {print("There are no replicate observations.")
                   print("Exact Lack of Fit Test is Not Applicable.")
                   print("For an approximation, try approx = TRUE")
}
   else {
   SS <- function (x) 
         {
          xbar <- mean(x)
          sum((x-xbar)^2)
         }
   
   SSRes <- summary(lm.obj)$sigma^2*(length(y)-p)
   SSPE <- sum(sapply(y.x, SS))
   SSLOF <- SSRes - SSPE
   dfLOF <- length(y) - dfPE - p
   df <- c(dfLOF, dfPE)
   ss <- c(SSLOF, SSPE)
   ms <- ss/df
   f  <- c(ms[1]/ms[2],NA)
   pv <- c(1 - pf(f,df[1],df[2]))
   pred.ratio <- c(diff(range(predict(lm.obj)))/sqrt(p*ms[2]/length(x)),NA)
   table <- data.frame(df, ss, ms, f, pv, pred.ratio)
   dimnames(table) <- list(c("Lack of Fit", "Pure Error"), c("Df", 
        "Sum Sq", "Mean Sq", "F value", "Pr(>F)", "prediction ratio"))
   structure(table, heading = c("Test of Lack of Fit for Simple Linear Regression\n", 
   paste("Response:", deparse(formula(lm.obj)[[2]]))), class = c("anova","data.frame"))
   } 
}

"partial.plot" <-
function (x, y, i=1) 
{
xnames <- names(x)
x <- as.matrix(x)
yname <- names(y)
y <- as.matrix(y)
xi.lm <- lm(x[,i] ~ x[,-i])
xi.res <- residuals(xi.lm)
y.lm <- lm(y ~ x[,-i])
y.res <- residuals(y.lm)
plot(xi.res, y.res, pch=16, xlab = xnames[i], ylab = yname,
main = "Partial Regression Plot" )
}

"softbacks" <-
structure(list(volume = c(412, 953, 929, 1492, 419, 1010, 595, 
1034), weight = c(250, 700, 650, 975, 350, 950, 425, 725)), .Names = c("volume", 
"weight"), class = "data.frame", row.names = c("8", "9", "10", 
"11", "12", "13", "14", "15"))
"geophones" <-
structure(list(distance = c(13.75, 15, 16.25, 17.5, 18.75, 20, 
21.25, 22.5, 23.75, 25, 26.25, 27.5, 28.75, 30, 31.25, 32.5, 
33.75, 35, 36.25, 37.5, 38.75, 40, 41.25, 42.5, 43.75, 45, 46.25, 
47.5, 48.75, 50, 51.25, 52.5, 53.75, 55, 56.25, 57.5, 58.75, 
61.25, 62.5, 63.75, 65, 66.25, 67.5, 68.75, 70, 71.25, 72.5, 
73.75, 75, 76.25, 77.5, 78.75, 80, 81.25, 82.5, 83.75), thickness = c(279, 
278, 280, 278, 275, 274, 276, 277, 276, 275, 277, 278, 276, 276, 
278, 276, 275, 273, 283, 279, 279, 278, 276, 276, 275, 273, 274, 
274, 275, 276, 275, 276, 276, 271, 274, 270, 270, 270, 273, 270, 
260, 267, 261, 260, 256, 251, 249, 248, 248, 250, 251, 253, 251, 
251, 254, 257)), .Names = c("distance", "thickness"), row.names = c("1", 
"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
"14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
"25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
"36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", 
"47", "48", "49", "50", "51", "52", "53", "55", "56", "57"), class = "data.frame")
"hills" <-
structure(list(dist = c(2.4, 6, 6, 7.5, 8, 8, 16, 6, 5, 6, 28, 
5, 9.5, 6, 4.5, 10, 14, 3, 4.5, 5.5, 3, 3.5, 6, 2, 3, 4, 6, 5, 
6.5, 5, 10, 6, 18, 4.5, 20), climb = c(650, 2500, 900, 800, 3070, 
2866, 7500, 800, 800, 650, 2100, 2000, 2200, 500, 1500, 3000, 
2200, 350, 1000, 600, 300, 1500, 2200, 900, 600, 2000, 800, 950, 
1750, 500, 4400, 600, 5200, 850, 5000), time = c(0.268055555555556, 
0.805833333333333, 0.560833333333333, 0.76, 1.03777777777778, 
1.22027777777778, 3.41027777777778, 0.606111111111111, 0.495833333333333, 
0.6625, 3.21111111111111, 0.7175, 1.08333333333333, 0.735555555555556, 
0.448888888888889, 1.20416666666667, 1.64027777777778, 1.31083333333333, 
0.290277777777778, 0.542777777777778, 0.265833333333333, 0.465, 
0.794166666666667, 0.298888888888889, 0.311388888888889, 0.436944444444444, 
0.573888888888889, 0.476111111111111, 0.841666666666667, 0.349166666666667, 
1.42638888888889, 0.539722222222222, 2.8375, 0.468333333333333, 
2.66388888888889)), .Names = c("dist", "climb", "time"), row.names = c("1", 
"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
"14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
"25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35"
), class = "data.frame")
"tomatoes" <-
structure(list(salinity = c(1.6, 1.6, 1.6, 1.6, 1.6, 3.8, 3.8, 
3.8, 3.8, 6, 6, 6, 6, 10.2, 10.2, 10.2, 10.2, 10.2), electrical.conductivity = c(59.5, 
53.3, 56.8, 63.1, 58.7, 55.2, 59.1, 52.8, 54.5, 51.7, 48.8, 53.9, 
49, 44.6, 48.5, 41, 47.3, 46.1)), .Names = c("salinity", "electrical.conductivity"
), row.names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
"10", "11", "12", "13", "14", "15", "16", "17", "18"), class = "data.frame")
"PRESS" <-
function (lm.obj)
{
sum((residuals(lm.obj)/(1-lm.influence(lm.obj)$hat))^2)
}

