function [hy,h] = plotseq(x,stateSeq,hmm)

% PLOTSEQ2 Plot a 2-dimensional sequence together with a state sequence
%
%    PLOTSEQ(X,STATESEQ) plots a two-dimensional sequence X (one observation
%    per row) as a 2-dimensional figure. It superposes the corresponding
%    state sequence STATESEQ as colored dots on the obesrvations. X and
%    STATESEQ must have the same length.
%
%    PLOTSEQ(X,STATESEQ,HMM) adds the "Gaussian ellipsis" corresponding to
%    each state of the model stored in the object HMM, where:
%       HMM.means = MEANS;
%       HMM.vars = VARS;
%       HMM.trans = TRANSITIONS
%

if nargin < 3,
  hmm = [];
end;

len = size(x,1);
if (len+2) ~= length(stateSeq),
  error('The length of the state sequence must be the length of the observation sequence plus 2.');
end;

stateSeq = stateSeq(2:(end-1));
numStates = max(stateSeq);

cmap = hsv(numStates-1);

hy(1) = plot(x(:,1),x(:,2),'y');
washold = ishold; hold on;
%set(gca,'xlim',[0 2500],'ylim',[500 3000],'dataAspectRatio',[1 1 1]);
set(gca,'dataAspectRatio',[1 1 1]);

for i=2:numStates,
  [where] = find(stateSeq ~= i);
  copy = x;
  copy(where,:) = NaN;
    
  h(i-1,1) = plot(copy(:,1),copy(:,2),'color',cmap(i-1,:), ...
      'marker','o','markerface',cmap(i-1,:), 'markerSize', 5, ...
      'linestyle','none');
   
  leg{i-1} = ['State ' num2str(i)];

end;

if ~isempty(hmm),
  for i=2:(length(hmm.means)-1),
    plotgaus(hmm.means{i},hmm.vars{i},cmap(i-1,:));
  end;
end;

if ~washold, hold off; end;

legend(h,leg);
