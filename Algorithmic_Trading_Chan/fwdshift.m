function y=fwdshift(day,x)
assert(day>=0);


y=[x(day+1:end,:, :); NaN*ones(day,size(x,2), size(x, 3))];
