% PURPOSE: An example using tvp_markov(),
%                           prt(),
%                           plt(),
% time-varying parameter model with Markov switching variances estimation
%---------------------------------------------------
% USAGE: tvp_markovd
%---------------------------------------------------

% Example taken from Kim and Nelson (1999)
% State-Space Models with Regime Switching

clear all;

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

% initial values
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
info.v0 = eye(k)*100;
info.start = 11;

result = tvp_markov(y,x,parm,info);

vnames = strvcat('m1 growth','constant','dinterest', ...
                 'inflation','surplus','m1lag');

prt(result,vnames);
plt(result,vnames);

