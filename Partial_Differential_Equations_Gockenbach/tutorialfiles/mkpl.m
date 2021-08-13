function pl = mkpl(x,v)

% pl = mkpl(x,v)
%
%   This function creates a continuous, piecewise linear function
%   interpolating the data (x(i),v(i)), i=1,...,length(x).  The
%   vectors x and v must be of the same length. Also, it is
%   assumed that the components of x are increasing.

m = length(x);
if m ~= length(v)
   error( 'Input vectors must have same length' );
end

coeffs = zeros(m-1,2);

for i=1:m-1

   ip1 = i+1;
   coeffs(i,1) = (v(ip1)-v(i))/(x(ip1)-x(i));
   coeffs(i,2) = v(i);
end

pl = mkpp(x,coeffs);
