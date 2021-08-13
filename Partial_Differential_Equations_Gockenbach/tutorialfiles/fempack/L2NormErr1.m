function m=L2NormErr1(T,fnu,U,g)

% m=L2NormErr1(T,fnu,U,g)
%
%   This function computes the L2 norm of the difference between
%   the smooth function u(x,y) and the piecewise linear function defined by
%   the mesh T and the nodal values U.  The function fnu implements u(x,y).
%
%   The optional input g is a vector containing the nodal values of the
%   piecewise linear function at the constrained nodes.  If g is not given,
%   these values are taken to be zero.
%
%   See "help Mesh1" for a description of the mesh T.

if nargin<4
   g=zeros(size(T.CNodePtrs));
end

m=0;

% Loop over the triangles in the mesh

for i=1:length(T.ElList)

   % Get the indices of the vertices of this triangle:

   kk=T.ElList(i,1:3);

   % Get the coordinates of the vertices:

   c=T.NodeList(kk,1:2);

   % Get the indices of the nodes in the list of free nodes:

   ll=T.NodePtrs(kk);

   % Collect the nodal values of the piecewise linear function:

   v=zeros(3,1);
   for j=1:3
      llj=ll(j);
      if llj>0
         v(j)=U(llj);
      else
         v(j)=g(-llj);
      end
   end

   % We must compute
   %
   %      integral over Ti (u(x,y)^2).
   %
   % We use a three-point quadrature rule.

   % Compute weight for the quadrature rule (1/3 times the
   % area of the triangle):

   J=[c(2,1)-c(1,1) c(3,1)-c(1,1);c(2,2)-c(1,2) c(3,2)-c(1,2)];
   A=(1/6)*abs(det(J));

   % The columns of M1 contain the values of the three basis functions
   % at the quadrature nodes:

   M2=(1/6)*[4 1 1;1 4 1;1 1 4];

   % Compute the quadrature nodes:

   q=M2*c;

   % Evaluate u(x,y) and the piecewise linear function at the quadrature nodes:

   uvals=feval(fnu,q(:,1),q(:,2));
   pwlvals=M2*v;

   % Compute the local integral:

   I=A*sum((uvals-pwlvals).^2);

   % Add the contribution to the global integral:

   m=m+I;

end

m=sqrt(m);

