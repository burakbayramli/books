% Create a uniform mesh of a rectangular domain
% Input: sx,sy: x- and y-coordinates of the domain's lower left corner
%        ex,ey: x- and y-coordinates of the upper right corner
% Output: nodes, elements: nodes and elements matrices 
function [nodes elements]=LShapeUniformMeshQuad4(sx,sy,ex,ey,nx,ny,sx2,sy2,ex2,ey2)
[x,y]=meshgrid(sx:(ex-sx)/nx:ex, sy:(ey-sy)/ny:ey);  % x,y matrices of
                                                     % the grid points
%nodes=zeros((nx+1)*(ny+1),3);  % empty nodes matrix
%elements=zeros(nx*ny,5);       % empty elements matrix
nids=zeros(ny+1,nx+1);         % empty node ID matrix
TOL=1e-9;                      % tolerance

% next 11 lines: create nodes
fid=fopen('nodes.dat','w+');   % open file "nodes.dat"
k=1;
for i=1:ny+1       % rows
  for j=1:nx+1     % columns
    xp=x(i,j);
    yp=y(i,j);
    if xp<sx2+TOL || xp>ex2-TOL || yp<sy2+TOL || yp>ey2-TOL
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

% next 13 lines: create elements
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