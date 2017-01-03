function pot = tabularPot(domain, sizes, T)
% TABULARPOT Make a tabular potential.
% pot = tabularPot(domain, sizes, T)
%
% sizes(i) is the size of the i'th domain element.
% T defaults to all 1s.


if nargin==0 % Used when objects are loaded from disk
  obj = init_fields;
  obj = class(obj, 'tabularPot');
  return;
end
firstArg = domain;
if isa(firstArg, 'tabularPot') %  used when objects are passed as arguments
  obj = firstArg;
  return;
end

pot.domain = domain(:)'; % so we can see it when we display
if nargin < 2
  sizes = 2*ones(length(domain));
end
if nargin < 3
  T = myones(sizes);
end
pot.T = T;
pot.sizes = sizes(:)';
pot = class(pot, 'tabularPot');
