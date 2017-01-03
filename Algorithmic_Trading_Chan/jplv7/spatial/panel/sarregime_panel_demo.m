% Demo of model with spatial and time period fixed effects and two regimes

% written by: J.Paul Elhorst 2/2007
% University of Groningen
% Department of Economics
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCES: 
% Elhorst J.P., Fréret S. (2009) Evidence of political yardstick competition in France 
% using a two-regime spatial Durbin model with fixed effects. 
% Journal of Regional Science. Forthcoming.

% Read data
A=xlsread('X:\freret\Data1.xls');
W=wk1read('X:\freret\weight.wk1');
% Row-normalize W
W=normw(W);
% Model parameters and y and x variables
N=93;
T=9;
nobs=N*T;
K=14;
y=A(:,17); % column number in the data matrix that corresponds to the dependent variable
dum=A(:,36); % column number in the data matrix that corresponds to the regime indicator
xh=A(:,[5,6,12,10,7,9,8]);% column numbers in the data matrix that correspond to the independent variables, no constant because it will be eliminated
% Create wx variables
for t=1:T
    t1=1+(t-1)*N;t2=t*N;
    wx(t1:t2,:)= W*xh(t1:t2,:);
end
x=[dum xh wx];
info.model=3;
results = sarregime_panel(y,x,dum,W,T,info);
vnames=strvcat('Socspend','dum','density','income','taxbase','unemployment','DGD','DGF','other grants','W*density','W*income','W*taxbase','W*unemployment','W*DGD','W*DGF','W*other grants');
prt_spreg(results,vnames,1);