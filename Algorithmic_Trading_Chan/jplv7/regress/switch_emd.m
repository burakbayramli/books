% PURPOSE: Demo of switch_em
%     EM-estimation of switching regime regression
%     and prt,plt
%---------------------------------------------------
% USAGE: switch_emd
%---------------------------------------------------

clear all;

% generate data from switching regression model
nobs = 100; n1 = 3; n2 = 3; n3 = 3;
b1 = ones(n1,1); b2 = ones(n2,1)*5; b3 = ones(n3,1);
sig1 = 1; sig2 = 1;
randn('seed',201010);
x1 = randn(nobs,n1); x2 = randn(nobs,n2); x3 = randn(nobs,n3);
ytruth = zeros(nobs,1);
for i=1:nobs;
 if x3(i,:)*b3 <= 0
  y(i,1) = x1(i,:)*b1 + randn(1,1);
  ytruth(i,1) = 0;
 else
  y(i,1) = x2(i,:)*b2 + randn(1,1);
  ytruth(i,1) = 1;
 end;
end;

result = switch_em(y,x1,x2,x3,b1,b2,b3);

vnames1 = strvcat('y1','x1_1','x1_2','x1_3');
vnames2 = strvcat('y2','x2_1','x2_2','x2_3');
vnames3 = strvcat('x3_1','x3_2','x3_3');
vnames = [vnames1
          vnames2
          vnames3];

prt(result,vnames);
plt(result,vnames);
pause;

[yts yind] = sort(ytruth);
prob2 = result.prob2(yind,1);
prob1 = result.prob1(yind,1);

tt=1:nobs;
subplot(2,1,1),
plot(tt,prob2,tt,yts,'--');
title('regime 2 probabilities vs truth');
xlabel('Observations sorted into regimes 1 and 2');
ylabel('probability');
subplot(2,1,2),
plot(tt,prob1,tt,ones(nobs,1) - yts,'--');
title('regime 1 probabilities vs truth');
xlabel('Observations sorted into regimes 1 and 2');
ylabel('probability');
pause;

nyts = ones(nobs,1) - yts;

fprintf(1,'prob1 truth prob2 truth \n');
for i=1:nobs;
    fprintf(1,'%6.2f %6.2f %6.2f %6.2f \n',prob1(i,1),nyts(i,1),prob2(i,1),yts(i,1));
end;

