### Computational model for PSI: contemporary US
data <- read.csv("PSImodel2020.csv")

w_0 = 1
mu_0 = 0.1
lambda = 0.5 


RelWage1 <- data$ProdWage/data$GDPpc
RelWage1 <- RelWage1/RelWage1[data$year == 1980]
RelWage2 <- data$UnskillWage/data$GDPpc
RelWage2 <- RelWage2/RelWage2[data$year == 1980]
data$RelWage <- (RelWage1+RelWage2)/2
