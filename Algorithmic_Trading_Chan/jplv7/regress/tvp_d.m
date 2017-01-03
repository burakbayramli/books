% PURPOSE: An example using tvp(),
%                           prt(),
%                           plt(),
% time-varying parameter model estimation
%---------------------------------------------------
% USAGE: tvp_d
%---------------------------------------------------

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

vnames = strvcat('m1 growth','constant','dinterest', ...
                 'inflation','surplus','m1lag');


priorb0 = zeros(k,1); % diffuse prior for b
priorv0 = eye(k)*50;  % diffuse prior for sigb


% do Zellner g-prior variant of the model
parm2 = [0.5 % sige
         1.0]; % delta
info2.delta = 1.0; % this turns on Zellner g-prior variant
info2.start = 11;
info2.b0 = priorb0; % match prior used above
info2.v0 = priorv0;
info2.prt = 1; % turn on printing intermediate optimization results

result2 = tvp(y,x,parm2,info2);

prt(result2,vnames);
plt(result2,vnames);

% do full-variant of the model
% initial values
parm = [0.5 % sige
        0.1 % sigb1
        0.1 % sigb2
        0.1 % .
        0.1 % .
        0.1];

%info.prt = 1; % turn off printing of brief
               % intermediate optimization results
info.start = 11; % start in 64Q1 (as in Kim-Nelson)

info.b0 = priorb0; % to match Kim-Nelson prior
info.v0 = priorv0;

result = tvp(y,x,parm,info);


prt(result,vnames);
% compare to Table 3.2 page 45

plt(result,vnames);
% compare to Figures 3.9 to 3.14


start = info.start;
% compare estimates
clf;
tt=start:n;
for i=1:k;
plot(tt,result.beta(:,i),tt,result2.beta(:,i),'--');
legend('tvp','tvp g-prior');
pause;
end;
