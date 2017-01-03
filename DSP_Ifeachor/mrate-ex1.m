%
% m-file for working out the computational
% complexities of a 2-stage decimator
% Program name: mrate-ex1.m
%
dp=0.01; ds=0.01; dp1=dp/2; ds1=ds;
fp=5000;
Fi=1536000; F0=12000;
M=Fi/F0; M1=16; M2=8;
F1=Fi/M1; F2=F1/M2;
fs1=F1-(Fi/(2*M)); fs2=F2-(Fi/(2*M));
df1=(fs1-fp)/Fi; df2=(fs2-fp)/F1;
NUM= -10*log10(dp1*ds1)-13;
N1=(NUM/(14.6*df1)); N2=(NUM/(14.6*df2));
MPS=(N1*F1 + N2*F2); TSR = N1+N2;
M1
M2
fs1
fs2
df1
df2
N1
N2
MPS
TSR


