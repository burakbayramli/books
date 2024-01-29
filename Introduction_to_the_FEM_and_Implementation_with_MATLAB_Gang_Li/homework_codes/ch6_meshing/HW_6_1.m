clear all;
b=100; a=50; c=20; d=60;   % input parameter
ex=b;
ey=a;

m=5; n=3; p=8; q=4;        % input parameter
nx=m+n;
ny=p+q;

[x,y]=meshgrid(0:ex/nx:ex, 0:ey/ny:ey);  % x,y matrices of
                                         % the grid points

% next block: nodal coordinates of the 3 blocks
[xa ya]=meshgrid(0:d/n:d, a-c:c/p:ey);
x(q+1:ny+1,1:n+1)=xa;
y(q+1:ny+1,1:n+1)=ya;
[xa ya]=meshgrid(d:(b-d)/m:ex, 0:(a-c)/q:a-c);
x(1:q+1,n+1:nx+1)=xa;
y(1:q+1,n+1:nx+1)=ya;
[xa ya]=meshgrid(0+d:(b-d)/m:ex, a-c:c/p:ey);
x(q+1:ny+1,n+1:nx+1)=xa;
y(q+1:ny+1,n+1:nx+1)=ya;

nids=zeros(ny+1,nx+1);         % empty node ID matrix
TOL=1e-9;                      % tolerance

% next block: create nodes
fid=fopen('nodes.dat','w+');   % open file "nodes.dat"
k=1;
for i=1:ny+1       % rows
  for j=1:nx+1     % columns
    xp=x(i,j);
    yp=y(i,j);
    if xp<-1+TOL || xp>d-TOL || yp<-1+TOL || yp>a-c-TOL
      nids(i,j)=k;   % grid matrix of node IDs
      nodes(k,1:3)=[k xp yp];  
      fprintf(fid,'%d %.10f %.10f\n',k, xp, yp); % write file
      k=k+1;
    else
      nids(i,j)=-1;   % node outside the domain
    end 
  end
end
fclose(fid);       % close "nodes.dat"

% next block: create elements
fid=fopen('elements.dat','w+'); % open file "elements.dat"
k=1;
for i=1:ny         % rows
  for j=1:nx       % columns
    if nids(i,j)>0 && nids(i,j+1)>0 && nids(i+1,j+1)>0 && nids(i+1,j)>0
      elements(k,1)=k;
      elements(k,2:3)=[nids(i,j) nids(i,j+1)];
      elements(k,4:5)=[nids(i+1,j+1) nids(i+1,j)]; 
      fprintf(fid,'%d %d %d %d %d \n', k, nids(i,j), ...
	           nids(i,j+1),nids(i+1,j+1),nids(i+1,j));
      k=k+1;
    end
  end
end
fclose(fid);  % close "elements.dat"
plotmesh;