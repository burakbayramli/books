function nnlist = fneighbors2(d, xcoord, ycoord, nnmax, delorder)
%
% fneighbors2(d, xcoord, ycoord, nnmax, delorder)
%
%This function finds the nnmax nearest neighbors to each observation. Users
%should set mmax to be equal or higher to the number of neighbors they will
%need in the future. 
%
%INPUT:
%
%A weight matrix d, such as coming from Delaunay triangles (e.g., fdelw2).
%This is n by n, and can be symmetric or asymmetric.
%
%The n by 1 locational vectors xcoord, ycoord
%
%The scalar nnmax which is the largest number of neighbors needed in future
%spatial regressions.
%
%The optional scalar delorder equals 1, 2, 3, or 4. This limits the extent to search for neighbors
%to the sum of the delaunay orders. In other words, delorder=2 limits the search to the delaunay
%neighbors and the neighbors of neighbors while delorder=4 limits the search to neighbors, neighbors
%neighbors, neighbors of neighbors of neighbors, and neighbors of neighbors of neighbors of neighbors!
%Lower values of delorder speed computation and reduce memory. The default is 2.
%
%OUTPUT:
%
%The disk file smats.mat which contains a sequence of nnmax sparse matrices each
%containing at most a single element 1 in each row. These are labeled s1,
%s2, s3, .... snnmax. The functions fsym_neighbors2 and fasym_neighbors2
%work with smat and its component files.
%
%NOTES:
%
%One can optimize the likelihood over neighbors and weighting of neighbors
%as in:
%
%Pace, R. Kelley and Ronald Barry, O.W. Gilley, C.F. Sirmans, 
%“A Method for Spatial-temporal Forecasting with an Application to Real Estate Prices,” 
%International Journal of Forecasting, Volume 16, Number 2, April-June 2000, p. 229-246.
%
%Written by Kelley Pace, www.spatial-statistics.com, 6/97 and revised
%12/26/02.

%obtaining the number of observations.
n=length(xcoord);

if nargin<5
    delorder=2;
end;

%This is an important step, and one can change this to obtain different
%behavior. This step assumes that the nearest neighbors come from the
%neighbors to each observation and the neighbors of the neighbors to each
%observation. If this is too low, higher order individual neighbor matrices
%will not have a element in some rows. If d comes from a Delaunay triangularization,
%this provides a hybrid of nearest neighbors and delaunay triangles. If this is too high, the
%computational cost can rise quickly. 

if (delorder<1)|(delorder>4)
    warning('setting delorder to 2 -- possible wrong argument');
end

if delorder==1
    poslist=(d>0)';
end

if delorder==2
    poslist=((d+d*d)>0)';
end

if delorder==3
    d2=d*d;
    poslist=((d+d2+d*d2)>0)';
    clear d2
end

if delorder==4
    d2=d*d;
    poslist=((d+d2+d*d2+d2*d2)>0)';
    clear d2
end

%d2=d*d;
%d3=d*d2;
%poslist=((d+d2+d3)>0)';%nearest neighbors within first, second, and third
%order lags
%poslist=((d+d2+d3+d*d3)>0)';%nearest neighbors within first, second,
%third, and fourth order lags.

%Each observation is a neighbor to itself. We wish to exclude this and find
%nnmax other neighbors
m1=nnmax+1;

%This block identifies all the neighbors to each observation, and sorts them by
%squared Euclidean distance which gives the same answers as Euclidean
%distance, but avoids a slight amount of computation. This is stored in
%nnlist, a n by nnmax matrix containing the observation indices of the
%different neighbors.
nnlist=zeros(n,m1);
nnseq=(1:n)';
for i=1:n
   plist=logical(poslist(:,i));
   nns=nnseq(plist);
   n_del_neighbors=length(nns);
   d=(xcoord(plist)-xcoord(i)).^2+(ycoord(plist)-ycoord(i)).^2;
   [ds,dind]=sort(d);
   if n_del_neighbors<m1;%sometimes there are fewer than nnmax different neighbors
      nnlist(i,1:n_del_neighbors)=nns(dind)';
      else;
         nnlist(i,:)=nns(dind(1:m1))';
         end;
end;
%We delete the own neighbor, and keep nnmax different neighbors
nnlist=nnlist(:,2:m1);

