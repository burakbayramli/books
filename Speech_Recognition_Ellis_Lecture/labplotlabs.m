function labplotlabs(T,L,K)
% labplotlabs(T,L,K)    Add time-aligned labels to a plot
%    T and L define time extents and label indices as returned by 
%    labreadlab.  K is an optional mapping of the label indices 
%    to label strings as returned by labreadkey.  Add the labels to 
%    the current plot in place of the x-axis.
% 2001-03-28 dpwe@ee.columbia.edu

if nargin < 2
  labs = num2str(T(:,2)');
elseif nargin < 3
  labs = num2str(L);
else
  [nsym,lsym] = size(K);
  % Add an 'unknown' symbol
  usym = ['?',' '*ones(1,lsym-1)];
  Kplus = [usym;K];
  % L == 0 gives us the usym
  labs = Kplus(L+1,:);
end

% Cute hack exploits matlab's labeling support.
set(gca,'XTickLabel',char(labs));
set(gca,'XTick',T(:,2)');
xlabel('Labels');
