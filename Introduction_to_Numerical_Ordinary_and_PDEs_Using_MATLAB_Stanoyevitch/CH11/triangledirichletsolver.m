function [Z, x, y] = triangledirichletsolver(n, leftdata, bottomdata, slantdata)
% This program will solve the Dirichlet problem of Laplaces equation
% on the special isoceles triangle with vertices (0,0), (1,0), (0,1).
% The finite difference method will be used. 
% The inputs are as follows: n = the number of interior grid points 
% on both the x- and y-axis (so n+2= total # of x/y-grid values).  
% leftdata = vector of boundary values on left side (size n+2, read top to
% bottom, bottomdata = vector of boundary values on bottom side (size n+2)
% slantdata = vector of boundary values on slant side (size n+2, read from top)
% The output variables are as follows:
% Z = the n+2 by n+2 matrix of the discrete solution's values
% x = vector of x grid values
% y = vector of y grid values (in reverse order to facilitate plots)
N=n*(n-1)/2; %number interior nodes (with unknown function values)
A=diag(4*ones(1,N)); 
border = [0];
count = 1;
for i=1:n-1
    border(i+1)=border(i)+count;
    count=count+1;
end

for k=2:length(border)
if k>2, pregap=right-left+1;, end
left=border(k-1)+1;, right=border(k);
if k<length(border)
postgap=right-left+1;
end
for i=left:right
if i<right %has right neighbor and top neighbor
A(i,[i+1   i-pregap])=-1;
end
if i>left %has left neighbor
A(i,i-1)=-1;
end
if k<length(border) %has bottom neighbor
A(i,i+postgap)=-1;
end
end
end

%Next we need to build the vector C
C=zeros(N,1);
for k=2:length(border)
left=border(k-1)+1;, right=border(k);
C(left)=C(left)+leftdata(k+1);
C(right)=C(right)+slantdata(k+1)+slantdata(k);
if k==length(border)
   for i=left:right
       C(i)=C(i)+bottomdata(i-left+2);
   end
end
end

U=A\C;

%start building the matrix of the numerical solution
Z=ones(n-1);
count=1;
for i=1:n-1
gap=border(i+1)-count+1;
Z(i,1:gap)=U(count:(count+gap-1))';
count=count+gap;
end
Z=[ones(1,n-1);[slantdata(2) ones(1,n-2)];Z;bottomdata(2:n)];
Z=[leftdata', Z, [ones(1,n+1) bottomdata(n+1)]', slantdata'];
for i=1:n+2
    Z(i,i)=slantdata(i);
end
%We delete those values of the Z matrix which are not in the triangle
%except for those nodes adjacent to two diagonal nodes where we use an
%average value
for i=1:n+2
    if i<n+2
    Z(i,i+1)=(Z(i,i)+Z(i+1,i+1))/2;
    end
    for j=i+2:n+2
        Z(i,j)=nan;
    end
end

h=1/(n+1);
x=0:h:1;, y=x;
for i=1:length(y), yrev(i)=y(length(y)+1-i);, end
y=yrev;

