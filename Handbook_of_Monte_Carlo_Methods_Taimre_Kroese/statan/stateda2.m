%stateda2.m
N = 10^3;
x = -log(rand(1,N)); 
y = -log(rand(1,N));
z = x + y; 
meanz = mean(z);
medz = median(z);
stdz = std(z);
varz = var(z);
maxz = max(z);
minz= min(z);
q1 = quantile(z,0.25);
q3 = quantile(z,0.75);
display([meanz,stdz, varz])
display([minz, q1, medz, q3,  maxz])
covyz = cov(y,z)
