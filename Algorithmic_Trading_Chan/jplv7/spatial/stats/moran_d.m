% PURPOSE: An example of using moran()
%          to test for spatial error correlation
%          on a small data set                   
%---------------------------------------------------
% USAGE: moran_d
%---------------------------------------------------


load anselin.dat;

y = anselin(:,1);
n = length(y);

x = [ones(n,1) anselin(:,2:3)];

xc = anselin(:,4);
yc = anselin(:,5);
[j W j] = xy2cont(xc,yc);

result = moran(y,x,W);
prt(result);


% demo non-standardized weight matrix
Wns = zeros(n,n);
for i=1:n
   for j=1:n
    if W(i,j) ~= 0
      Wns(i,j) = 1.0;
    end;
   end;
end;

result = moran(y,x,Wns);
prt(result);


