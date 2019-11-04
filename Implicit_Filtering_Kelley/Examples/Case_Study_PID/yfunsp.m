function yp=yfunsp(t,y,pid_parms)
%
% simple harmonic oscillator for parameter id example
%
% system form of y'' + c y' + k y = 0
%
yp=zeros(2,1);
yp(1)=y(2);
c=pid_parms(1);
k=pid_parms(2);
yp(2)= - k* y(1) - c*y(2);

