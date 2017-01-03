function out=lndetint(wsw,rmin,rmax)
% PURPOSE: computes Pace and Barry's spline approximation to log det(I-rho*W)
% -----------------------------------------------------------------------
% USAGE: out = lndetint(W)
% where:    
%             W     = symmetric spatial weight matrix (standardized)
% -----------------------------------------------------------------------
% RETURNS: out = a structure variable
%          out.lndet = a vector of log determinants for 0 < rho < 1
%          out.rho   = a vector of rho values associated with lndet values
% -----------------------------------------------------------------------
% NOTES: only produces results for a grid of 0 < rho < 1
%        where the grid ranges by 0.01 increments
% -----------------------------------------------------------------------
% References: % R. Kelley Pace and Ronald P. Barry "Simulating Mixed Regressive
% Spatially autoregressive Estimators", Computational Statistics, 1998,
% Vol. 13, pp. 397-418.
% -----------------------------------------------------------------------
 
% This function computes a vector of log-determinants for a vector 
% of AR parameters (alpha)
% It uses a spline interpolation routine to reduce the number of 
% determinants computed
% Written by Kelley Pace, 3/19/98
% (named fdetinterpasym1.m in the spatial statistics toolbox )

% Documentation and output format changed by J. LeSage
% for consistency with the Econometrics Toolbox 12/99

if nargin == 1
rmin = 0;
rmax = 1;
end;

spparms('tight');
%spparms('autommd',0);
%these settings help optimize the sparse matrix routines

c=wsw;

[n,n]=size(c);
s1=speye(n);
z=s1-.1*c;
p=colamd(z);
%this is the symmetric minimum degree ordering


iter=100;
alphavec=((1:iter)-1)/iter;
%selecting points to use for the interpolation
alphaselect=[ 10 20 40 50 60  70  80 85 90 95 96 97 98 99 100];
itersub=length(alphaselect);

detsub=zeros(itersub,1);
for i=1:itersub;
alpha=alphavec(alphaselect(i));
z=s1-alpha*c;
[l,u]=lu(z(:,p));
%LU decomposition
detsub(i)=sum(log(abs(diag(u))));
end;

%stores grid and log-determinant for later use
%interpolating for finer grid of alpha
out.lndet = interp1([0;alphavec(alphaselect)'],[0;detsub],alphavec,'spline')';
out.rho = alphavec';


