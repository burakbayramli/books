function m=L2Norm1(T,p1,p2)

% m=L2Norm1(T,'fnu')
%
%   or
%
% m=L2Norm1(T,U,g)
%
%   This function estimates the L2 norm of either a smooth function
%   or a piecewise linear function by integrating over the triangles in
%   the mesh T. See "help Mesh1" for a description of the mesh T.
%
%   In the case of a smooth function u(x,y), 'fnu' implements this function.
%
%   In the case of a piecewise linear function, U is a vector containing
%   the nodal values of u(x,y).  The optional vector g contains the values
%   of u(x,y) at the constrained nodes (i.e. the Dirichlet data).  If
%   g is not given, these values are taken to be zero.

if isnumeric(p1)
   if nargin<3
      p2=zeros(size(T.CNodePtrs));
   end
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

   % Evaluate u(x,y) at the quadrature nodes:

   if ~isnumeric(p1)
      % Compute the quadrature nodes:

      q=M2*c;
      uvals=feval(p1,q(:,1),q(:,2));
   else
      % Collect the nodal values of the piecewise linear function:

      v=zeros(3,1);
      for j=1:3
         llj=ll(j);
         if llj>0
            v(j)=p1(llj);
         else
            v(j)=p2(-llj);
         end
      end

      uvals=M2*v;
   end

   % Compute the local integral:

   I=A*(uvals'*uvals);

   % Add the contribution to the global integral:

   m=m+I;

end

m=sqrt(m);

