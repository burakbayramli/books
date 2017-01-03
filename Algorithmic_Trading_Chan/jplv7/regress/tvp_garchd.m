% PURPOSE: An example using tvp_garch(),
%                           prt(),
%                           plt(),
% time-varying parameter model with garch(1,1) errors
%---------------------------------------------------
% USAGE: tvp_garchd
%---------------------------------------------------

% Example taken from Kim and Nelson (1999)
% State-Space Models with Regime Switching

load tvpgrch.data;


% column1: m1===growth rate of quarterly average M1
%  2: dint=change in the lagged interest rate (3-month T-bill)
%  3: inf==lagged inflation
%  4: surpl==lagged full employment budget surplus
%  5: m1lag==lag of m1
%     1959.3--1987.4, 

y = tvpgrch(:,1);
n = length(y);
x = [ones(n,1) tvpgrch(:,2:5)];
[n k] = size(x);

% initial values
parm = [0.52 % sigb0
        0.52 % sigb1
        0.52 % sigb2
        0.54 % .
        0.551% sigb4
        0.5    % a0
        0.5    % a1
        0.15]; % a2
          
info.b0 = zeros(k+1,1); % relatively diffuse prior     
info.v0 = eye(k+1)*50;  % to match Kim-Nelson

info.prt = 1; % turn on printing of some 
              %intermediate optimization results
info.start = 11; % starting observation

result = tvp_garch(y,x,parm,info);

vnames = strvcat('m1 growth','constant','dinterest', ...
                 'inflation','surplus','m1lag');

prt(result,vnames);
% compare to Table 6.1 page 145

plt(result,vnames);
% compare to Figure 6.1a

