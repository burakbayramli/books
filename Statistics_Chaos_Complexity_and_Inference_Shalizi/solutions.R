###### Functions for problem 1

# Non-zero fixed point of the logistic map
fixed.point = function(r) {(4*r-1)/(4*r)}

logistic.map.derivative = function(x,r) {4*r*(1-2*x)}

# Stability criterion for the fixed point of the logistic map as a function of r
stability.logistic.map.fp = function(r) {
   abs(logistic.map.derivative(fixed.point(r),r))
}

# Location of one of the points on the logistic map's 2-cycle
x.cycle.p = function(r) {
   0.5*(2 - fixed.point(r) 
        + sqrt(-3*fixed.point(r)*fixed.point(r) 
               +4*fixed.point(r)+4 -4*(4*r+1)/(4*r)))
}

# Location of the other one of the points on the logistic map's 2-cycle
x.cycle.m = function(r) {
   0.5*(2 - fixed.point(r) 
        - sqrt(-3*fixed.point(r)*fixed.point(r) 
               +4*fixed.point(r)+4 -4*(4*r+1)/(4*r)))
}

# Stability criterion for the 2-cycle as a function of r
stability.lm.2cycle = function(r) {
  abs(logistic.map.derivative(x.cycle.p(r),r) * 
      logistic.map.derivative(x.cycle.m(r),r))
}



######## Functions for problem 2

# Do one iteration of the rotation map
  # Notes:
  # 1. Through the magic of R vectorization, if given a vector of initial
  #    conditions, it will iterate them all in parallel
  # 2. Added optional argument "circle" for the measure of a full circle, so
  #    you can use radians (or whatever) rather than degrees if you want to
# Inputs: vector of angles (theta), angular increment (alpha), measure of
#         complete circle (circle, defaults 360)
# Output: vector of new angles
rotation.map <- function(theta,alpha, circle=360) {
  new.theta = (theta + alpha) %% circle  # "%%" is the modulus operator
  return(new.theta)
}

# Produce a rotation map time series
# Inputs: number of steps (timelength), angular increment (alpha), initial
#         condition (initial.cond, default is uniform random), measure of
#         complete circle (circle, defaults 360)
# Calls: rotation.map()
# Output: vector of length timelength
rotation.map.ts <- function(timelength,alpha,initial.cond=NULL,circle=360) {
  theta = vector(mode="numeric",length=timelength)
  if (is.null(initial.cond)) {
    theta[1] = runif(1,0,circle)
  } else {
    theta[1] = initial.cond
  }
  for (t in 2:timelength) {
    theta[t] = rotation.map(theta[t-1],alpha,circle)
  }
  return(theta)
}

# Evolve an initial ensemble according to the rotation map
# Inputs: number of time steps (timelength), angular increment (alpha),
#         vector of initial conditions (theta), measure of a full circle
#         (circle, default 360)
# Calls: rotation.map
# Outputs: vector of final conditions
rotation.map.evolution <- function(timelength,alpha,theta,circle=360) {
  for (t in 1:timesteps) {
    theta <- rotation.map(theta,alpha,circle)
  }
  return(theta)
}
