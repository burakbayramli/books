function out = tally(x)
% PURPOSE: calculate frequencies of distinct levels in x
%---------------------------------------------------
% USAGE:     out = tally(x)
% where:     x = an (nobs x nvar) matrix or vector
%---------------------------------------------------
% RETURNS:
%        out = (:,2) matrix
%        out(:,1) = distinct levels of x (sorted)
%        out(:,2) = counts corresponding to distinct levels
% --------------------------------------------------
% SEE ALSO: levels, indicator
%---------------------------------------------------

% Gordon K Smyth, 22 Jan 90, 29 Jul 97
% gks@maths.uq.edu.au
% Department of Mathematics, Unversity of Queensland, Q 4072, Australia
% http://www.maths.uq.edu.au/~gks/

% documentation modified by J.P. LeSage 1/98
% NOTE ALSO, this function returns two columns, not rows

%TALLY TALLY(X) calculates the frequencies of the distinct levels of X.
% Output is a matrix with two rows, the first containing the
% levels of X and the second the corresponding counts.

% GKS  27 Dec 1992

s=sort(x(:))';
[m n]=size(s);
i=find( [ 1 diff(s) ] > 0 );
out=[ s(i)' diff([i n+1])' ];
