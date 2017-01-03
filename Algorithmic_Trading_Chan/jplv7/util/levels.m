function f = levels(nlevels,nreps,length)
% PURPOSE: produces a variable vector of factor levels 
%---------------------------------------------------
% USAGE:     vfact  = levels(nlevels,nreps,length)
% where:     nlevels = # of levels
%            nreps   = # of repeated levels
%            length  = length of returned vector
%---------------------------------------------------
% RETURNS:
%        vfact = kron(1:nlevels,ones(nreps,1) repeated to
%                create a vector of length elements
% --------------------------------------------------
% SEE ALSO: indicator, tally
%---------------------------------------------------

% Gordon K Smyth, 22 Jan 90, 29 Jul 97
% gks@maths.uq.edu.au
% Department of Mathematics, Unversity of Queensland, Q 4072, Australia
% http://www.maths.uq.edu.au/~gks/

% documentation modified by J.P. LeSage 1/98

n=ceil( length/(nlevels*nreps) );
f=kron( ones(n,1) , kron( (1:nlevels)' , ones(nreps,1) ) );
f=f(1:length);
