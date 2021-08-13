function ShowPWLinFcn1(T,U,g)

% ShowPWLinFcn1(T,U,g)
%
%   This function draws a surface plot of a piecewise linear
%   function defined on a triangular mesh.  The inputs are T,
%   the mesh (see "help Mesh1" for details about this data structure)
%   and the vector U, giving the nodal values of the function
%   (typically U would be obtained by solving a finite element
%   equation).
%
%   The optional input g is a vector of nodal values at the
%   constrained nodes.  If g is not given, this vector is taken
%   to be zero.

if nargin<3
   g=zeros(length(T.CNodePtrs),1);
end

X = T.NodeList(:,1);
Y = T.NodeList(:,2);
Z = zeros(length(X),1);
Z(T.FNodePtrs) = U;
Z(T.CNodePtrs) = g;
tri = T.ElList(:,1:3);

trimesh(tri,X,Y,Z,'LineWidth',2)
