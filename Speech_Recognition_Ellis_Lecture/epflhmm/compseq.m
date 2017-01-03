function [hy,h] = compseq(x,stateSeq,bestSeq)

% COMPSEQ Comparison of two alignment of a random sequence
%
%    COMPSEQ(X,STATESEQ,BESTSEQ) plots a comparison between the original
%    state sequence STATESEQ and the Viterbi-determined best sequence
%    BESTSEQ for the sequence X. Only the first dimension of X is plotted.
%

len = size(x,1);
if (len+2) ~= length(stateSeq),
  error('The length of the state sequence must be the length of the observation sequence plus 2.');
end;
if length(bestSeq) ~= length(stateSeq),
  error('The compared state sequences must have the same length.');
end;

stateSeq = stateSeq(2:(end-1));
numStates = max(stateSeq);
bestSeq = bestSeq(2:(end-1));

cmap = hsv(numStates-1);

subplot(2,1,1);
hy(1) = plot(x(:,1),'y');
xlabel('Original alignment');
hold on;

subplot(2,1,2);
hy(2) = plot(x(:,1),'y');
xlabel('Viterbi alignment');
hold on;

for i=2:numStates,
  [where] = find(stateSeq ~= i);
  copy = x;
  copy(where,:) = NaN;

  subplot(2,1,1);
  h(i-1,1) = plot(copy(:,1),'color',cmap(i-1,:), ...
      'marker','o','markerface',cmap(i-1,:),'linestyle','none');
  
  [where] = find(bestSeq ~= i);
  copy = x;
  copy(where,:) = NaN;

  subplot(2,1,2);
  h(i-1,2) = plot(copy(:,1),'color',cmap(i-1,:), ...
      'marker','o','markerface',cmap(i-1,:),'linestyle','none');
  
  leg{i-1} = ['State ' num2str(i)];
end;

where = find(stateSeq~=bestSeq);
if ~isempty(where),
  subplot(2,1,1);
  h(numStates,1) = plot(where, x(where,1),'y', ...
      'marker','o','markersize',10, ...
      'linestyle','none','linewidth',2);
  subplot(2,1,2);
  h(numStates,2) = plot(where, x(where,1),'y', ...
      'marker','o','markersize',10, ...
      'linestyle','none','linewidth',2);
  leg{numStates} = ['Misalignment'];
else,
  h(numStates,1) = plot(NaN,NaN,'linestyle','none');
  h(numStates,2) = plot(NaN,NaN,'linestyle','none');
  leg{numStates} = ['NO misalignment detected'];
end;

subplot(2,1,1);
legend(h,leg);