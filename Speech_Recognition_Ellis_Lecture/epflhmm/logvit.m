function [stateSeq,logProb] = logvit(x,means,vars,transitions)

% LOGVIT Log version of the Viterbi algorithm applied to alignment
%
%    [STATESEQ,LOGPROB] = LOGVIT(X,MEANS,VARS,TRANSITIONS) returns the best
%    state sequence and the likelihood of the 2-dimensional sequence X (one
%    observation per row) with respect to a Markov model with N states
%    having means MEANS and variances VARS (stored in N elements lists with
%    empty matrices as first and last elements to symbolize the entry and
%    exit states) and transition matrix TRANSITIONS.
%      Alternately, LOGVIT(X,HMM) can be used, where HMM is an object of the
%    form:
%       HMM.means = MEANS;
%       HMM.vars = VARS;
%       HMM.trans = TRANSITIONS
%

if nargin == 2,
  model = means;
  means = model.means;
  vars = model.vars;
  evalc( 'logTrans = log(model.trans);' );
end;

numStates = length(means);
nMinOne = numStates - 1;
[numPts,dim] = size(x);

log2pi = log(2*pi);
for i=2:nMinOne,
  invSig{i} = inv(vars{i});
  logDetVars2(i) = - 0.5 * log(det(vars{i})) - log2pi;
end;

% Initialize the delta and psi vectors for the emitting states
for i=2:nMinOne,
  X = x(1,:)-means{i}';
  delta(i) = logTrans(1,i) ...
      - 0.5 * (X * invSig{i}) * X' + logDetVars2(i);
  psi(1,i) = 1;
end;
delta = delta(:);

% Recursion
for t = 2:numPts,
  deltaBefore = delta;
  for i = 2:nMinOne,
    X = x(t,:)-means{i}';
    [maxDelta,index] = max( deltaBefore(2:nMinOne) + logTrans(2:nMinOne,i) );
    if index == 0; disp('argh'); end;
    delta(i) = max( maxDelta ) - 0.5 * (X * invSig{i}) * X' + logDetVars2(i);
    psi(t,i) = index + 1;
  end;
end;

% Termination
stateSeq(numPts+2) = numStates; % Exit state
[maxDelta,index] = max( delta(2:nMinOne) + logTrans(2:nMinOne,numStates) );
logProb = maxDelta;
stateSeq(numPts+1) = index + 1;

% Backtracking
for t = numPts:-1:2,
  stateSeq(t) = psi(t,stateSeq(t+1));
end;
stateSeq(1) = 1; % Entry state