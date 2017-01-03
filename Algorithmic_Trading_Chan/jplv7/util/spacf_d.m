% PURPOSE: demo of spacf() 
%          sample partial autocorrelation function estimates
% 
%---------------------------------------------------
% USAGE: spacf_d
%---------------------------------------------------


nobs = 110;

% generate arma(2,1) model data

e = randn(nobs,1);

y = zeros(nobs,1);

for i=3:nobs;
y(i,1) =  10 +  0.8*y(i-1,1) -0.4*y(i-2,1) + e(i,1) + 0.25*e(i-1,1);
end;

yt = y(11:nobs,1);

m = 30;

acf = sacf(y,m);
pause;

pacf = spacf(y,m);
pause;

tt=1:m;
ttp = tt';

fprintf(1,'Lag     acf        pacf \n');
for i=1:m
fprintf(1,'%2d %10.4f %10.4f \n',ttp(i,1),acf(i,1),pacf(i,1));
end;
