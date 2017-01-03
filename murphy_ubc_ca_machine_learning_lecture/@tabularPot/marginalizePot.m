function smallpot = marginalizePot(bigpot, onto, maximize)
% MARGINALIZEPOT Marginalize a dpot onto a smaller domain.
% smallpot = marginalizePot(bigpot, onto, maximize)
%
% 'onto' must be in ascending order.

if nargin < 3, maximize = 0; end

ns = zeros(1, max(bigpot.domain));
ns(bigpot.domain) = bigpot.sizes;
smallT = marg_table(bigpot.T, bigpot.domain, bigpot.sizes, onto, maximize);
smallpot = tabularPot(onto, ns(onto), smallT);

%%%%%%%%%%


function smallT = marg_table(bigT, bigdom, bigsz, onto, maximize)
% MARG_TABLE Marginalize a table
% smallT = marg_table(bigT, bigdom, bigsz, onto, maximize)

if nargin < 5, maximize = 0; end

smallT = myreshape(bigT, bigsz); % make sure it is a multi-dim array
sum_over = mysetdiff(bigdom, onto);
ndx = find_equiv_posns(sum_over, bigdom);
if maximize
  for i=1:length(ndx)
    smallT = max(smallT, [], ndx(i));
  end
else
  for i=1:length(ndx)
    smallT = sum(smallT, ndx(i));
  end
end


ns = zeros(1, max(bigdom));
%ns(bigdom) = mysize(bigT); % ignores trailing dimensions of size 1
ns(bigdom) = bigsz;

smallT = squeeze(smallT); % remove all dimensions of size 1
smallT = myreshape(smallT, ns(onto)); % put back relevant dims of size 1
