# Exhibit 2.1
win.graph(width=4.875, height=2.5,pointsize=8)
# rwalk contains a simulated random walk
data(rwalk)
plot(rwalk,type='o',ylab='Random Walk')

# R code for simulating a random walk with, say 60, iid standard normal errors
n=60
set.seed(12345) # intialize the random number so that the simulation can be 
# reproducible.
sim.random.walk=ts(cumsum(rnorm(n)),freq=1,start=1)
plot(sim.random.walk,type='o',ylab='Another Random Walk')

