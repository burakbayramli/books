% PURPOSE: An example using tvp_markov(), and tvp_garch()
%          Compares the estimates from both models
%          See Kim and Nelson (1999)
% time-varying parameter model with Markov switching variances 
% and time-varying parameter model with garch(1,1) errors
%---------------------------------------------------
% USAGE: tvp_markovd2
%---------------------------------------------------

clear all;

% Example taken from Kim and Nelson (1999)
% State-Space Models with Regime Switching

load tvpmoney.data;

% column 1 = quarter index
%  2: m1===growth rate of quarterly average M1
%  3: dint=change in the lagged interest rate (3-month T-bill)
%  4: inf==lagged inflation
%  5: surpl==lagged full employment budget surplus
%  6: m1lag==lag of m1
%     1959.3--1987.4, 

y = tvpmoney(:,2);
n = length(y);
x = [ones(n,1) tvpmoney(:,3:6)];
[n k] = size(x);

% initial values for tvp_markov model
parm = [0.5 % p Pr(St=1, St-1=1)
        0.5 % q pr(St=0, St-1=0)
        0.5 % sigb std deviations for transition equation
        0.5 % .
        0.5 % .
        0.5 % .
        0.5 %
        0.5 % sige in state 1
        1.0]; % sige in state 2];

info.prt = 1; % turn on printing of brief
              % intermediate optimization results

info.b0 = zeros(k,1); % to match Kim-Nelson prior
info.v0 = eye(k)*50;
info.start = 11;

result = tvp_markov(y,x,parm,info);

vnames = strvcat('m1 growth','constant','dinterest', ...
                 'inflation','surplus','m1lag');

% initial values for tvp_garch model
parm2 =[0.5 % sigb0
        0.5 % sigb1
        0.5 % sigb2
        0.5 % .
        0.5% sigb4
        0.5    % a0
        0.5    % a1
        0.15]; % a2
          
info2.b0 = zeros(k+1,1); % relatively diffuse prior     
info2.v0 = eye(k+1)*50;  % to match Kim-Nelson

info2.prt = 1; % turn on printing of some 
              %intermediate optimization results
info2.start = 11; % starting observation

result2 = tvp_garch(y,x,parm2,info2);

cstruct = cal(1959,3,4);

starto = cal(cstruct,info.start); % find date of starting observation

% produce calendar structure based on this date
cstruct = cal(starto.year,starto.period,starto.freq);

% do time-series plot of each parameter estimate from both models
% NOTE: we have two sets of estimates for the tvp_markov model
for j=1:result.nvar;
 tmp  = strcat('state0  ', vnames(j+1,:));
 tmp2 = strcat('state1  ', vnames(j+1,:));
vname = strvcat(tmp,tmp2,'tvp garch');
tsplot([result.beta1(:,j) result.beta2(:,j) result2.beta(:,j)],cstruct,vname);
title('tvp beta comparisons');
pause;
end;

% compare the conditional forecast errors
% See figure 5.5, page 120 in Kim-Nelson
total = result.fvar(:,1) + result.fvar(:,2);
ferror = [total result2.fvar];
vname = strvcat('tvp Markov','tvp garch');
tsplot(ferror,cstruct,vname);
title('forecast error variance comparisons');
pause;

% compare residuals
vname = strvcat('tvp Markov','tvp garch');
tsplot([result.resid result2.resid],cstruct,vname);
title('residual comparisons');

