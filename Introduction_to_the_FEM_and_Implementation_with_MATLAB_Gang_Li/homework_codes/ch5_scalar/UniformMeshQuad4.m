% Create a uniform mesh of a rectangular domain
% Input: sx,sy: x- and y-coordinates of the domain's lower left corner
%        ex,ey: x- and y-coordinates of the upper right corner
% Output: nodes, elements: nodes and elements matrices 
function [nodes,elements]=UniformMeshQuad4(sx,sy,ex,ey,nx,ny)
[x,y]=meshgrid(sx:(ex-sx)/nx:ex, sy:(ey-sy)/ny:ey);  % x,y matrices of
                                                     % the grid points
nodes=zeros((nx+1)*(ny+1),3);  % empty nodes matrix
elements=zeros(nx*ny,5);       % empty elements matrix
nids=zeros(ny+1,nx+1);         % empty node ID matrix

% next 11 lines: create nodes
fid=fopen('nodes.dat','w+');   % open file "nodes.dat"
k=1;
for i=1:ny+1       % rows
  for j=1:nx+1     % columns
    nids(i,j)=k;   % grid matrix of node IDs
    nodes(k,1:3)=[k x(i,j) y(i,j)];  
    fprintf(fid,'%d %.10f %.10f\n',k, x(i,j), y(i,j)); % write file
    k=k+1;
  end
end
fclose(fid);       % close "nodes.dat"

% next 13 lines: create elements
fid=fopen('elements.dat','w+'); % open file "elements.dat"
k=1;
for i=1:ny         % rows
  for j=1:nx       % columns
    elements(k,1)=k;
    elements(k,2:3)=[nids(i,j) nids(i,j+1)];
    elements(k,4:5)=[nids(i+1,j+1) nids(i+1,j)]; 
    fprintf(fid,'%d %d %d %d %d \n', k, nids(i,j), ...
	          nids(i,j+1),nids(i+1,j+1),nids(i+1,j));
    k=k+1;
  end
end
fclose(fid);  % close "elements.dat"