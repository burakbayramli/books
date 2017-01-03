% 
% Bandpass filter length estimation
% Ncoeff2.m Problem 7.5, p426.
%
df=3/96;
dp=0.00115; ds=0.0001;
b1=0.01202; b2=0.09664; b3=-0.51325;
b4=0.00203; b5=-0.5705; b6=-0.44314;
t1=log10(dp);
t2=log10(ds);
cps=t2*(b1*t1*t1+b2*t1+b3)+(b4*t1*t1+b5*t1+b6);
gps=-14.6*log10(dp/ds)-16.9;
N=(cps/df)+gps*df+1;
cps
gps
N
df
