# personality-data example for BDA2, analyzing data from just 6 persons
# (to save space in the figure)

# data from Kristof Vansteelandt; see Vansteelandt, K., & Van Mechelen, I. (1998).  Individual differences in situation-behavior profiles: A triple typology model.  Journal of Personality and Social Psychology, 75, 751-765. 

nsubjects <- 6
nrep <- 7

test <- function (a){
  output <- as.vector(a)>0
  glm.data.frame <- data.frame (output, response, situation, person)
  glm0 <- glm (output ~
    factor(response) + factor(situation) + factor(person),
    family=binomial(link=logit),
    data=glm.data.frame)
  pred0 <- predict.glm (glm0, type="response")
  mean (ifelse(output, (1-pred0)^2, pred0^2))
}

data <- array (scan("personality.dat"), c(15,23,54))
data <- data[,,1:nsubjects]
dims <- dim(data)
data <- ifelse (data>0, 1, 0)
av <- banova(data, c("response", "situation", "person"))
round (av$banova, 2)
reorder <- as.list (rep(NA,3))
for (k in 1:3)
  reorder[[k]] <- order (av$coef[[1]][[k]])

output <- as.vector(data)
output <- output>0
response <- as.vector(slice.index(data,1))
situation <- as.vector(slice.index(data,2))
person <- as.vector(slice.index(data,3))
glm.data.frame <- data.frame (output, response, situation, person)
glm0 <- glm (output ~
  factor(response) + factor(situation) + factor(person),
  family=binomial(link=logit),
  data=glm.data.frame)
pred0 <- predict.glm (glm0, type="response")
yrep <- array (NA, c(dim(data),nrep))
for (rep in 1:nrep){
  yrep[,,,rep] <- ifelse (runif(length(output))<pred0, 1, 0)
}

postscript ("unordered3.ps")
plot (c(-10,(dims[2]+10)*9), c(10,-(dims[1]+10)*nsubjects), type="n",
  xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty="n")
for (i in 1:dims[3]){
  xpos <- 0
  ypos <- -(dims[1]+10)*(i-1)
  image (xpos + (1:dims[2]), ypos - (1:dims[1]),
    t(data[,,i]), zlim=c(0,1), add=T)
}
polygon (c(-5, -5, -5+(dims[2]+10), -5+(dims[2]+10)),
  c(5, 5-(dims[1]+10)*nsubjects, 5-(dims[1]+10)*nsubjects, 5), density=0)
for (rep in 1:nrep){
  yrep0 <- yrep[,,,rep]
  for (i in 1:dims[3]){
    xpos <- (dims[2]+10)*(rep+1)
    ypos <- -(dims[1]+10)*(i-1)
    image (xpos + (1:dims[2]), ypos - (1:dims[1]),
      t(yrep0[,,i]), zlim=c(0,1), add=T)
  }
  polygon (-5+(dims[2]+10)*c(rep+1, rep+1, rep+2, rep+2),
    c(5, 5-(dims[1]+10)*nsubjects, 5-(dims[1]+10)*nsubjects, 5), density=0)
}
dev.off()

postscript ("raw3.ps")
plot (c(-10,(dims[2]+10)*9), c(10,-(dims[1]+10)*nsubjects), type="n",
  xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty="n")
for (i in 1:dims[3]){
  xpos <- 0
  ypos <- -(dims[1]+10)*(i-1)
  image (xpos + (1:dims[2]), ypos - (1:dims[1]),
    t(data[reorder[[1]],reorder[[2]],reorder[[3]][i]]), zlim=c(0,1), add=T)
}
polygon (c(-5, -5, -5+(dims[2]+10), -5+(dims[2]+10)),
  c(5, 5-(dims[1]+10)*nsubjects, 5-(dims[1]+10)*nsubjects, 5), density=0)
for (rep in 1:nrep){
  yrep0 <- yrep[,,,rep]
  av <- banova(yrep0, c("response", "situation", "person"))
  round (av$banova, 2)
  reorder <- as.list (rep(NA,3))
  for (k in 1:3)
    reorder[[k]] <- order (av$coef[[1]][[k]])
  for (i in 1:dims[3]){
    xpos <- (dims[2]+10)*(rep+1)
    ypos <- -(dims[1]+10)*(i-1)
    image (xpos + (1:dims[2]), ypos - (1:dims[1]),
      t(yrep0[reorder[[1]],reorder[[2]],reorder[[3]][i]]), zlim=c(0,1), add=T)
  }
  polygon (-5+(dims[2]+10)*c(rep+1, rep+1, rep+2, rep+2),
    c(5, 5-(dims[1]+10)*nsubjects, 5-(dims[1]+10)*nsubjects, 5), density=0)
}
dev.off()

postscript ("antisymm3.ps")
plot (c(-10,(dims[2]+10)*9), c(10,-(dims[1]+10)*nsubjects), type="n",
  xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty="n")

for (rep in 1:nrep){
  yrep0 <- data - yrep[,,,rep]
  av <- banova(yrep0, c("response", "situation", "person"))
  round (av$banova, 2)
  reorder <- as.list (rep(NA,3))
  for (k in 1:3)
    reorder[[k]] <- order (av$coef[[1]][[k]])
  for (i in 1:dims[3]){
    xpos <- (dims[2]+10)*(rep+1)
    ypos <- -(dims[1]+10)*(i-1)
    image (xpos + (1:dims[2]), ypos - (1:dims[1]),
      t(yrep0[reorder[[1]],reorder[[2]],reorder[[3]][i]]), zlim=c(-1,1),add=T)
  }
  polygon (-5+(dims[2]+10)*c(rep+1, rep+1, rep+2, rep+2),
    c(5, 5-(dims[1]+10)*nsubjects, 5-(dims[1]+10)*nsubjects, 5), density=0)
}
dev.off()

# replicate with noise!

data.noise <- yrep[,,,1]
av <- banova(data.noise, c("response", "situation", "person"))
round (av$banova, 2)
reorder <- as.list (rep(NA,3))
for (k in 1:3)
  reorder[[k]] <- order (av$coef[[1]][[k]])

output <- as.vector(data.noise)
output <- output>0
response <- as.vector(slice.index(data.noise,1))
situation <- as.vector(slice.index(data.noise,2))
person <- as.vector(slice.index(data.noise,3))
glm.data.frame <- data.frame (output, response, situation, person)
glm0 <- glm (output ~
  factor(response) + factor(situation) + factor(person),
  family=binomial(link=logit),
  data=glm.data.frame)
pred0 <- predict.glm (glm0, type="response")
yrep <- array (NA, c(dim(data.noise),nrep))
for (rep in 1:nrep){
  yrep[,,,rep] <- ifelse (runif(length(output))<pred0, 1, 0)
}

postscript ("raw3_noise.ps")
plot (c(-10,(dims[2]+10)*9), c(10,-(dims[1]+10)*nsubjects), type="n",
  xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty="n")
for (i in 1:dims[3]){
  xpos <- 0
  ypos <- -(dims[1]+10)*(i-1)
  image (xpos + (1:dims[2]), ypos - (1:dims[1]),
    t(data.noise[reorder[[1]],reorder[[2]],reorder[[3]][i]]), zlim=c(0,1), add=T)
}
polygon (c(-5, -5, -5+(dims[2]+10), -5+(dims[2]+10)),
  c(5, 5-(dims[1]+10)*nsubjects, 5-(dims[1]+10)*nsubjects, 5), density=0)
for (rep in 1:nrep){
  yrep0 <- yrep[,,,rep]
  av <- banova(yrep0, c("response", "situation", "person"))
  round (av$banova, 2)
  reorder <- as.list (rep(NA,3))
  for (k in 1:3)
    reorder[[k]] <- order (av$coef[[1]][[k]])
  for (i in 1:dims[3]){
    xpos <- (dims[2]+10)*(rep+1)
    ypos <- -(dims[1]+10)*(i-1)
    image (xpos + (1:dims[2]), ypos - (1:dims[1]),
      t(yrep0[reorder[[1]],reorder[[2]],reorder[[3]][i]]), zlim=c(0,1), add=T)
  }
  polygon (-5+(dims[2]+10)*c(rep+1, rep+1, rep+2, rep+2),
    c(5, 5-(dims[1]+10)*nsubjects, 5-(dims[1]+10)*nsubjects, 5), density=0)
}
dev.off()

postscript ("antisymm3_noise.ps")
plot (c(-10,(dims[2]+10)*9), c(10,-(dims[1]+10)*nsubjects), type="n",
  xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty="n")

for (rep in 1:nrep){
  yrep0 <- data.noise - yrep[,,,rep]
  av <- banova(yrep0, c("response", "situation", "person"))
  round (av$banova, 2)
  reorder <- as.list (rep(NA,3))
  for (k in 1:3)
    reorder[[k]] <- order (av$coef[[1]][[k]])
  for (i in 1:dims[3]){
    xpos <- (dims[2]+10)*(rep+1)
    ypos <- -(dims[1]+10)*(i-1)
    image (xpos + (1:dims[2]), ypos - (1:dims[1]),
      t(yrep0[reorder[[1]],reorder[[2]],reorder[[3]][i]]), zlim=c(-1,1),add=T)
  }
  polygon (-5+(dims[2]+10)*c(rep+1, rep+1, rep+2, rep+2),
    c(5, 5-(dims[1]+10)*nsubjects, 5-(dims[1]+10)*nsubjects, 5), density=0)
}
dev.off()
