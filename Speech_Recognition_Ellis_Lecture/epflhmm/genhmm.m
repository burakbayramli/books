function [x,stateSeq] = genhmm(means,vars,transitions)

% GENHMM Generation of a random sequence from a Markov Model
%
%    [X,STATESEQ] = GENHMM(MEANS,VARS,TRANSITIONS) returns a 2-dimensional
%    sequence X (one observation per row) and a state sequence STATESEQ
%    drawn from a Markov model with N states having means MEANS and
%    variances VARS (stored in N elements lists with empty matrices as first
%    and last elements to symbolize the entry and exit states) and
%    transition matrix TRANSITIONS.
%      Alternately, GENHMM(HMM) can be used, where HMM is an object of the
%    form:
%       HMM.means = MEANS;
%       HMM.vars = VARS;
%       HMM.trans = TRANSITIONS
%


if nargin == 1,
  model = means;
  means = model.means;
  vars = model.vars;
  transitions = model.trans;
end;

dim = length(means{2});
numStates = length(means);

for i=2:(numStates-1),
  stDevs{i} = sqrtm(vars{i});
end;
stDevs{1} = [];
stDevs{numStates} = [];

% Generate the emitting states sequence
stateSeq(1) = 1; % Begin with entry state
t = 1;
while (stateSeq(t) ~= numStates),
  t = t+1;
  stateSeq(t) = pickState( transitions(stateSeq(t-1),:) );
end;

% Pick values in emitting states pdfs, omitting the entry state
for t = 2:(length(stateSeq)-1);
  x(t-1,:) = randn(1,dim) * stDevs{stateSeq(t)} + means{stateSeq(t)}';
end;


%=============================================
function [stat] = pickState(localTransitions)

cs = cumsum(localTransitions);
if ( (cs(end) - 1.0) > (eps/2) ),
  error('Bad transition probability values given to subroutine pickState.');
end;

unif = rand;
stat = 1;
while ( unif >= cs(stat) ),
  stat = stat+1;
end;
