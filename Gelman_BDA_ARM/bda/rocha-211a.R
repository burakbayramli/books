y         = c(-2,-1,0,1.5,2.5);
m         = 100;
n.samples = 1e4;
t.grid = matrix(seq(1/(2*m),1-(1/(2*m)),by=1/m), ncol=1);

d.grid = matrix(rep(NA,length(t.grid)), ncol=1);
for (i in 1:length(t.grid))
{
  d.grid[i] = prod(dcauchy(y, location = t.grid[i]));
}

print (d.grid)