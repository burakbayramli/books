function T=RectangleMeshN1(nx,ny,lx,ly)

% T=RectangleMeshN1(nx,ny,lx,ly)
%
%   This function creates a regular, nx by ny finite element mesh for
%   a Neumann problem on the rectangle [0,lx]x[0,ly].
%
%   The last three arguments can be omitted; their default values are
%   ny=nx, lx=1, ly=1.  Thus, the command "T=RectangleMeshN1(m)" creates
%   a regular mesh with 2m^2 triangles on the unit square.
%
%   For a description of the data structure describing T, see "help Mesh1".

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

N=M;
T.FNodePtrs=zeros(N,1);

% Compute the number of constrained nodes and allocate space for CNodePtrs:

K=0;
T.CNodePtrs=[];

% Compute the number of triangles and allocate space for ElList, ElEdgePtrs:

L=2*nx*ny;
T.ElList=zeros(L,3);
T.ElEdgeList=zeros(L,3);

% Compute the number of boundary edges and allocate space for FBndyList:

B=2*nx+2*ny;
T.FBndyList=[(1:nx)',(2:nx+1)'
(nx+1:nx+1:ny*(nx+1))',(2*(nx+1):nx+1:(ny+1)*(nx+1))'
((ny+1)*(nx+1):-1:ny*(nx+1)+2)',((ny+1)*(nx+1)-1:-1:ny*(nx+1)+1)'
(ny*(nx+1)+1:-(nx+1):nx+2)',((ny-1)*(nx+1)+1:-(nx+1):1)'
];
nfb=0;

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
      k=k+1;

      % Insert the coordinates of the node

      T.NodeList(k,1:2)=[x,y];

      % All nodes are free

      kf=kf+1;
      T.NodePtrs(k)=kf;
      T.FNodePtrs(kf)=k;

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

      k=k+2;

      % Recall that the entries in each row of ElEdgeList are
      % flags indicating whether the edges lie on the boundary or not.

      if i==1
         nfb=nfb+1;
         f1=nfb;
      else
         f1=0;
      end
      if j==ny
         nfb=nfb+1;
         f2=nfb;
      else
         f2=0;
      end
      T.ElList(k,:)=[(j-1)*(nx+1)+i,j*(nx+1)+i,j*(nx+1)+i+1];
      T.ElEdgeList(k,:)=[f1,f2,0];
      if j==1
         nfb=nfb+1;
         f1=nfb;
      else
         f1=0;
      end
      if i==nx
         nfb=nfb+1;
         f2=nfb;
      else
         f2=0;
      end
      T.ElList(k+1,:)=[(j-1)*(nx+1)+i,(j-1)*(nx+1)+i+1,j*(nx+1)+i+1];
      T.ElEdgeList(k+1,:)=[f1,f2,0];

   end

end

