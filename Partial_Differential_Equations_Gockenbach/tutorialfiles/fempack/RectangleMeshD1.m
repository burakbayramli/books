function T=RectangleMeshD1(nx,ny,lx,ly)

% T=RectangleMeshD1(nx,ny,lx,ly)
%
%   This function creates a regular, nx by ny finite element mesh for
%   a Dirichlet problem on the rectangle [0,lx]x[0,ly].
%
%   The last three arguments can be omitted; their default values are
%   ny=nx, lx=1, ly=1.  Thus, the command "T=RectangleMeshD1(m)"
%   creates a regular mesh with 2m^2 triangles on the unit square.
%
%   For a description of the data structure describing T, see
%   "help Mesh1".

% Assign default arguments:

if nargin<4
   ly=1;
end
if nargin<3
   lx=1;
end
if nargin<2
   ny=nx;
end

% Compute the number of nodes and allocate space for NodeList, NodePtrs:

M=(nx+1)*(ny+1);
T.NodeList=zeros(M,2);
T.NodePtrs=zeros(M,1);

% Compute the number of free nodes and allocate space for FNodePtrs:

N=(nx-1)*(ny-1);
T.FNodePtrs=zeros(N,1);

% Compute the number of constrained nodes and allocate space for CNodePtrs:

K=M-N;
T.CNodePtrs=zeros(K,1);

% Compute the number of triangles and allocate space for ElList, ElEdgeList:

L=2*nx*ny;
T.ElList=zeros(L,3);
T.ElEdgeList=zeros(L,3);

% Compute the number of boundary edges and allocate space for FBndyList:

B=0;
T.FBndyList=[];


% Loop over the rows and columns of the mesh, defining the nodes and
% free node pointers.
%
% k is the number of the node.
% kf is the number of the free node.
% kc is the number of the constrained node.

k=0;
kf=0;
kc=0;
dx=lx/nx;
dy=ly/ny;

% Loop over the rows of the grid

for j=0:ny

   y=j*dy; 

   % Loop over the columns of the grid

   for i=0:nx

      x=i*dx;
      k=k + 1;

      % Insert the coordinates of the node

      T.NodeList(k,1:2)=[x,y];

      % Interior nodes are free

      if ~((j==0)|(j==ny)|(i==0)|(i==nx))
         kf=kf+1;
         T.NodePtrs(k)=kf;
         T.FNodePtrs(kf)=k;
      else
         kc=kc+1;
         T.NodePtrs(k)=-kc;
         T.CNodePtrs(kc)=k;
      end

   end

end

% Loop over the rows and columns of the mesh, defining the triangular
% elements
%
% k is the number of the element

k=-1;

% Loop over the rows of the grid

for j=1:ny

   % Loop over the columns of the grid

   for i=1:nx

      % k is the number of the "upper left" triangle
      % k+1 is the number of the "lower right" triangle

      k=k + 2;

      % Recall that the three entries in ElEdgeList are
      % flags indicating whether the edges lie on the boundary or not.

      T.ElList(k,:)=[(j-1)*(nx+1)+i,j*(nx+1)+i,j*(nx+1)+i+1];
      T.ElEdgeList(k,:)=[-(i==1),-(j==ny),0];
      T.ElList(k+1,:)=[(j-1)*(nx+1)+i,(j-1)*(nx+1)+i+1,j*(nx+1)+i+1];
      T.ElEdgeList(k+1,:)=[-(j==1),-(i==nx),0];

   end

end
