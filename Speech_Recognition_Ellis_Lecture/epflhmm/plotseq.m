function [hy,h] = plotseq(x,stateSeq)

% PLOTSEQ Plot a 2-dimensional sequence together with a state sequence
%
%    PLOTSEQ(X,STATESEQ) plots a two-dimensional sequence X (one observation
%    per row) as two separate figures, one per dimension. It superposes the
%    corresponding state sequence STATESEQ as colored dots on the
%    obesrvations. X and STATESEQ must have the same length.
%

len = size(x,1);
if (len+2) ~= length(stateSeq),
  error('The length of the state sequence must be the length of the observation sequence plus 2.');
end;

stateSeq = stateSeq(2:(end-1));
numStates = max(stateSeq);

cmap = hsv(numStates-1);

subplot(2,1,1);
hy(1) = plot(x(:,1),'y');
xlabel('First dimension');
hold on;

subplot(2,1,2);
hy(2) = plot(x(:,2),'y');
xlabel('Second dimension');
hold on;

for i=2:numStates,
  [where] = find(stateSeq ~= i);
  copy = x;
  copy(where,:) = NaN;

  subplot(2,1,1);
  h(i-1,1) = plot(copy(:,1),'color',cmap(i-1,:), ...
      'marker','o','markerface',cmap(i-1,:),'linestyle','none');
  
  subplot(2,1,2);
  h(i-1,2) = plot(copy(:,2),'color',cmap(i-1,:), ...
      'marker','o','markerface',cmap(i-1,:),'linestyle','none');
  
  leg{i-1} = ['State ' num2str(i)];

end;

subplot(2,1,1);
legend(h,leg);