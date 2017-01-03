function y = smartsum(x,dim)
%SMARTSUM   Sum ignoring NaN.
%
%   Same as SUM except that it returns the sum of the finite elements
%   instead of propagating NaN and Inf
%   Returns NaN if no element is finite

if nargin==1, 
  % Determine which dimension SUM will use
  dim = min(find(size(x)~=1));
  if isempty(dim), dim = 1; end
  k=isfinite(x);
  %   x(~k)=-9999*ones(size(x(~k)));
  %   warning off
  %   y=sum(x.*k)./(sum(k,dim)>0);
  %   warning on
  x(~k)=0;
  y=sum(x, dim);
  y(sum(k, dim)==0)=NaN;

else
    %   k=isfinite(x);
    %   x(~k)=-9999*ones(size(x(~k)));
    %   warning off
    %   y=sum(x.*k,dim)./(sum(k,dim)>0);
    %   warning on

    k=isfinite(x);
    x(~k)=0;
    y=sum(x, dim);
    y(sum(k, dim)==0)=NaN;
    
end
