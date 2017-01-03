function [wswdel,wwsdel,wmatdel]=fdelw2(xcoord,ycoord)
% PURPOSE: %This function creates a spatial weight matrices based
% upon a Delaunay triangularization.
% USAGE: [wswdel,wwsdel,wmatdel]=fdelw2(xcoord,ycoord)
%
%
%INPUT:
%
%The variables xcoord and ycoord are n by 1 vectors containing locational
%coordinates.
%
%OUTPUT:
%
%The n by n matrix wmat has element i,i equalling 1/sqrt(sum of the
%elements in the ith row).
%
%The n by n matrix wwsdel equals wmat*wmat*A which yields a row-stochastic
%spatial weight matrix, where A represents the adjacency matrix from
%Delaunay triangles (Voronoi tesselation).
%
%The n by n matrix wswdel equals wmat*A*wmat which yields a symmetric
%spatial weight matrix.
%
%Both wwsdel and wswdel are similar (have the same eigenvalues) with maximum eigenvalue equalling 1
%and minimum eigenvalue greater than or equal to -1.0.
%
%NOTES:
%
%Written by Kelley Pace, www.spatial-statistics.com, originally on 6/23/97
%and revised on 12/25/02.

%number of locational pairs
n=length(xcoord);

%Calling Matlab internal delaunay function
tri=delaunay(xcoord,ycoord);

%Save some memory by deleting the locational coordinates
clear xcoord ycoord;

%This block of commands is taking the triplets of observations indices in
%tri and converting this into an adjacency matrix. There are 6 possible
%pairs implied by the triplets. I explicity for 3 of these and use
%symmetry for the others.
%
%This lines sets up a vector with number of elements equalt to length of
%try plus 1. The first elements equal 1, and the last element equals 0.
o=[ones(length(tri(:,1)),1);0];
%This sets non-zero values in the adjacency matrix corresponding to one of
%the unique pairs, and places a 0 at element n,n. The last part is
%important because it forces the matrix to be n by n.
bigma=spconvert([[tri(:,1:2);[n n]] o]);
%I use symmetry to obtain the other pair.
s=bigma+bigma';
%I no longer need bigma, so I discard it.
clear bigma;
%The rest of the block just follows the same process as the first part of
%the block.
bigmb=spconvert([[tri(:,2:3);[n n]] o]);
s=s+bigmb+bigmb';
clear bigmb;
bigmc=spconvert([[tri(:,[1 3]);[n n]] o]);
s=s+bigmc+bigmc';
clear bigmc;

%clears tri after no longer required to save memory
clear tri;

%converts to 0,1 matrix
s=(s>0);

%I find the row-sums
srowsum=sum(s');
%I normalize based on the row sums
zip = find(srowsum == 0); % avoids division by zero
srowsum(1,zip) = 1;
whalf=sqrt((1./srowsum)');

%creates diagonal scaling matrix
wmatdel=spdiags(whalf,0,n,n);
%creates row-stochastic weight matrix
wwsdel=wmatdel*wmatdel*s;
%creates symmetric matrix with max eigenvalue of 1
%see Ord (JASA, 1975) for more on this scaling
wswdel=wmatdel*s*wmatdel;

