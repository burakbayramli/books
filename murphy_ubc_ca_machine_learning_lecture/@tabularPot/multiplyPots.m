function T = multiplyPots(varargin)
% MULTIPLYPOTS Multiply tabular pots together pointwise.
% T = multiplyPots(pots1, pots2, ...)

N = length(varargin);
dom = [];
for i=1:N
  Ti = varargin{i};
  dom = [dom Ti.domain];
end
dom = unique(dom);
ns = zeros(1, max(dom));
for i=1:N
  Ti = varargin{i};
  ns(Ti.domain) = Ti.sizes;
end
T = tabularPot(dom, ns(dom));
for i=1:N
  Ti = varargin{i};
  T = multiplyByPot(T, Ti);
end

