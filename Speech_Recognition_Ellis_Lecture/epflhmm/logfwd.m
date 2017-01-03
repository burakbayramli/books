function logProb = logfwd(x,means,vars,transitions)

% LOGFWD Log version of the forward procedure
%
%    LOGPROB = LOGFWD(X,MEANS,VARS,TRANSITIONS) returns the likelihood of
%    the 2-dimensional sequence X (one observation per row) with respect to
%    a Markov model with N states having means MEANS and variances VARS
%    (stored in N elements lists with empty matrices as first and last
%    elements to symbolize the entry and exit states) and transition matrix
%    TRANSITIONS.
%      Alternately, LOGFWD(X,HMM) can be used, where HMM is an object of the
%    form:
%       HMM.means = MEANS;
%       HMM.vars = VARS;
%       HMM.trans = TRANSITIONS
%

if nargin == 2,
  model = means;
  means = model.means;
  vars = model.vars;
  model.trans(model.trans<1e-100) = 1e-100;
  logTrans = log(model.trans);
end;

numStates = length(means);
nMinOne = numStates - 1;
[numPts,dim] = size(x);

log2pi = log(2*pi);
for i=2:nMinOne,
  invSig{i} = inv(vars{i});
  logDetVars2(i) = - 0.5 * log(det(vars{i})) - log2pi;
end;

% Initialize the alpha vector for the emitting states
for i=2:nMinOne,
  X = x(1,:)-means{i}';
  alpha(i) = logTrans(1,i) ...
      - 0.5 * (X * invSig{i}) * X' + logDetVars2(i);
end;
alpha = alpha(:);

% Do the forward recursion
for t = 2:numPts,
  alphaBefore = alpha;
  for i = 2:nMinOne,
    X = x(t,:)-means{i}';
    alpha(i) = logsum( alphaBefore(2:nMinOne) + logTrans(2:nMinOne,i) ) ...
	- 0.5 * (X * invSig{i}) * X' + logDetVars2(i);
  end;
end;

% Terminate the recursion with the final state
logProb =  logsum( alpha(2:nMinOne) + logTrans(2:nMinOne,numStates) );

%=================================
function result = logsum(logv)

len = length(logv);
if (len<2);
  error('Subroutine logsum cannot sum less than 2 terms.');
end;

% First two terms
if (logv(2)<logv(1)),
  result = logv(1) + log( 1 + exp( logv(2)-logv(1) ) );
else,
  result = logv(2) + log( 1 + exp( logv(1)-logv(2) ) );
end;

% Remaining terms
for (i=3:len),
  term = logv(i);
  if (result<term),
    result = term   + log( 1 + exp( result-term ) );
  else,
    result = result + log( 1 + exp( term-result ) );
  end;    
end;
