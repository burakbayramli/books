y         = c(-2,-1,0,1.5,2.5);
m         = 100;
n.samples = 1e4;
t.grid = matrix(seq(1/(2*m),1-(1/(2*m)),by=1/m), ncol=1);

d.grid = matrix(rep(NA,length(t.grid)), ncol=1);
for (i in 1:length(t.grid))
{
  d.grid[i] = prod(dcauchy(y, location = t.grid[i]));
}

F.vec                = cumsum(d.grid)/sum(d.grid);
U                    = runif(n.samples);
sampled.indices      = matrix(rep(NA, n.samples), ncol=1);
for (j in 1:length(U))
{
  sampled.indices[j]=sum(F.vec<U[j])+1; # Get the index that was sampled from t.grid
}

# sample a uniform jitter about the sampled point
uniform.jitter       = matrix(runif(n.samples), ncol=1)*(1/(2*m));

posterior.sampled.t = t.grid[sampled.indices] + uniform.jitter;

jpeg("problem_2_11_b_bayesian_data_analysis.jpg", height=800, width=800);

hist(posterior.sampled.t, breaks=100 ,lwd=3, col="lightgrey", xlab = "theta",
      main = "histogram of p(theta|y1, ..., y5)");

posterior.sampled.y = posterior.sampled.t + rcauchy(n.samples);

jpeg("problem_2_11_c_bayesian_data_analysis.jpg", height=800, width=800);

hist(posterior.sampled.y, breaks=100 ,lwd=3, col="lightgrey", xlab = "theta",
     main = "histogram of p(y6|y1, ..., y5)");
     
dev.off();
