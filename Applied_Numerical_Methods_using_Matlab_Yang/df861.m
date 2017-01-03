function dx=df861(t,x)
global A
NA=size(A,2); 
if length(x)~=2*NA, error('Some dimension problem'); end
dx=[zeros(NA) eye(NA); -A zeros(NA)]*x(:);
if size(x,2)>1, dx=dx.'; end
