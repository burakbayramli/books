function y = smartcumsum(x,dim)
% y=SMARTCUMSUM(x, dim)   cumulative sum ignoring NaN.
%

x(~isfinite(x))=0;
y=cumsum(x);

