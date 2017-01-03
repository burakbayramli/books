function like = tvp_markov_lik(parm,y,x,start,b0,v0)
% PURPOSE: log-likelihood for Markov-switching TVP model 
% model: y(t) = X(t) b(t) + e(t)
%        b(t) = A_st b(t-1) + G_st v(t)
%        e(t) = N(0,R), v(t) = N(0_k,Q)
% -------------------------------------------------
% USAGE: like = tvp_markov_lik(parm,y,x,b0,v0)
% where: parm = a vector with parameters
%        parm(1,1) = p, Pr[St=1 | St-1=1]
%        parm(2,1) = q, Pr[St=0 | St-1=0]
%        parm(3:3+k-1,1) = diagonal transition equation variance
%        parm(3+k,1)   = noise std in state 1
%        parm(3+k+1,1) = noise std in state 2
%        y = dependent variable (nx1) vector
%        x = (nxk) matrix of explanatory variables
%        b0 = (kx1) prior b0 (default: diffuse = 0)
%        v0 = (kxk) prior v0 (default: diffuse = 1000)
% -------------------------------------------------
% RETURNS: -log likelihood        
% -------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[n k] = size(x);
IK = eye(k);

if nargin == 3
priorb0 = zeros(k,1);
priorv0 = IK*1000;
start = 1;
elseif nargin == 4
priorb0 = zeros(k,1);
priorv0 = IK*1000;
elseif nargin == 6
priorb0 = b0;
priorv0 = v0;
else
error('tvp_markov_lik: Wrong # of arguments');
end;


parm = ham_trans(parm); % 1/(1+exp(-parm))

ppr = parm(1,1); % Pr[St=1 | St-1=1]
qpr = parm(2,1); % Pr[St=0 | St-1=0]

aa = eye(k);

prob1 = (1-qpr)/(2-ppr-qpr); % Pr[St-1=1 | Yt-1], STEADY STATE PROB
prob0 = 1-prob1;             % Pr[ST-1=0 | Yt-1], STEADY STATE PROB

b0 = priorb0;
b1 = b0;
v0 = priorv0;
v1 = v0;

tmp = parm(3:3+k-1,1); % transition equation std deviations
tmp2 = tmp.*tmp;
qq = diag(tmp2);

sig0 = parm(3+k,1);
sig1 = parm(3+k+1,1);

like = 0;


for i=1:n;

H = x(i,:);

% prediction
postb00 = aa*b0; % for St-1 = 0, St = 0
postb01 = aa*b0; % for St-1 = 0, St = 1
postb10 = aa*b1; % for St-1 = 1, St = 0
postb11 = aa*b1; % for St-1 = 1, St = 1

postv00 = aa*v0*aa' + qq;
postv01 = aa*v0*aa' + qq;
postv10 = aa*v1*aa' + qq;
postv11 = aa*v1*aa' + qq;

fcast00 = y(i,1) - H*postb00;
fcast01 = y(i,1) - H*postb01;
fcast10 = y(i,1) - H*postb10;
fcast11 = y(i,1) - H*postb11;

ss00 = H*postv00*H' + sig0*sig0;
ss01 = H*postv01*H' + sig1*sig1;
ss10 = H*postv10*H' + sig0*sig0;
ss11 = H*postv11*H' + sig1*sig1;

% detect possible problems
if (ss00 <= 0 | ss01 <= 0 | ss10 <= 0 | ss11 <= 0)
        [ss00 ss01 ss10 ss11]
        fprintf(1,'negative variance in tvp_markov_lik \n');
        pause;
end;



kg00 = postv00*H' / ss00;
kg01 = postv01*H' / ss01;
kg10 = postv10*H' / ss10;
kg11 = postv11*H' / ss11;

b00 = postb00 + kg00*fcast00;
b01 = postb01 + kg01*fcast01;
b10 = postb10 + kg10*fcast10;
b11 = postb11 + kg11*fcast11;

v00 = (IK - kg00*H)*postv00;
v01 = (IK - kg01*H)*postv01;
v10 = (IK - kg10*H)*postv10;
v11 = (IK - kg11*H)*postv11;

prv00 = vprob(fcast00,ss00)*qpr*prob0; % pr[St, Yt | Yt-1]
prv01 = vprob(fcast01,ss01)*(1-qpr)*prob0;
prv10 = vprob(fcast10,ss10)*(1-ppr)*prob1;
prv11 = vprob(fcast11,ss11)*ppr*prob1;

prval = prv00+prv01+prv10+prv11; % pr[Yt | Yt-1]

pr00 = prv00/prval; 
pr01 = prv01/prval;
pr10 = prv10/prval;
pr11 = prv11/prval;

prob0 = pr00 + pr10; % pr[St=0 | Yt]
prob1 = pr01 + pr11; % pr[St=1 | Yt]

% avoid division by zero in cases
% where all probability weight gets placed on one state
if prob0 == 0
        prob0 = 0.001;
elseif prob0 == 1
        prob0 = 0.999;
end;
if prob1 == 0
        prob1 = 0.001;
elseif prob1 == 1
        prob1 = 0.999;
end;


% collapse terms eq 2.13' from Kim's article

b0 = (pr00*b00 + pr10*b10)/prob0;
b1 = (pr10*b10 + pr11*b11)/prob1;

fcast0 = (pr00*fcast00 + pr10*fcast10)/prob0;
fcast1 = (pr10*fcast10 + pr11*fcast11)/prob1;

% eq 2.14'
v0 = (pr00*(v00+(b0-b00)*(b0-b00)') ...
          +pr10*(v10+(b0-b10)*(b0-b10)'))/prob0;
v1 = (pr01*(v01+(b1-b01)*(b1-b01)') ...
          +pr11*(v11+(b1-b11)*(b1-b11)'))/prob1;


if i >= start
like = like - log(prval);
end;

end; % end of loop over observations




