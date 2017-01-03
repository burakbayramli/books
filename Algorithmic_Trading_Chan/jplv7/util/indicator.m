function ind = indicator(x,setto0)
% PURPOSE: converts the matrix x to indicator variables
%          associated with the levels in x
%---------------------------------------------------
% USAGE:     ind = indicator(x,omit)
% where:     x = a matrix or vector
%         omit = column # to be omitted in return matrix
%---------------------------------------------------
% RETURNS:
%        ind = levels rounded down to integers
% --------------------------------------------------
% SEE ALSO: levels, tally
%---------------------------------------------------

% Gordon K Smyth, 22 Jan 90, 29 Jul 97
% gks@maths.uq.edu.au
% Department of Mathematics, Unversity of Queensland, Q 4072, Australia
% http://www.maths.uq.edu.au/~gks/

% documentation modified by J.P. LeSage 1/98

x=floor(x(:));
xmin=min(x); xmax=max(x); xrange=xmax-xmin;
ind=( x*ones(1,xrange+1) )==( ones(size(x))*(xmin:xmax) );
ind=ind(:,any(ind));
if nargin>1
   ind(:,setto0)=[];
end;
