function varargout=assign(v)
%ASSIGN Assigns values to variables
% varargout=assign(v)
%
% assigns multiple numbers to variables:
% eg [a b]=assign([1 2]), [c d e]=assign([3 4 5])
for i=1:length(v); varargout{i}=v(i); end