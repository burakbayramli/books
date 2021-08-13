function m=EnergyNorm1(T,fnk,p1,p2)

% m=EnergyNorm1(T,fnk,fnux,fnuy)
%
%   or
%
% m=EnergyNorm1(T,fnk,U,g)
%
%   This function estimates the energy norm of either a smooth function
%   or a piecewise linear function by integrating over the triangles in
%   the mesh T. See "help Mesh1" for a description of the mesh T.  The
%   input fnk is the weight in the energy inner product; it can be
%   a (positive) number or a function k(x,y).
%
%   In the case of a smooth function u(x,y), fnux and fnuy are
%   the partial derivatives of v(x,y).  Each of these functions must
%   be a real-valued function of two real variables.
%
%   In the case of a piecewise linear function, U is a vector containing
%   the nodal values of u(x,y).  The optional vector g contains the values
%   of u(x,y) at the constrained nodes (i.e. the Dirichlet data).  If
%   g is not given, these values are taken to be zero.

if isnumeric(p1)
   if nargin<4
      p2=zeros(size(T.CNodePtrs));
   end
end

m=0;

% Loop over the triangles in the mesh

vo=ones(3,1);
for i=1:length(T.ElList)

   % Get the indices of the vertices of this triangle:

   kk=T.ElList(i,1:3);

   % Get the coordinates of the vertices:

   c=T.NodeList(kk,1:2);

   % Get the indices of the nodes in the list of free nodes:

   ll=T.NodePtrs(kk);

   % Compute the area of the triangle:

   J=[c(2,1)-c(1,1) c(3,1)-c(1,1);c(2,2)-c(1,2) c(3,2)-c(1,2)];
   A=0.5*abs(det(J));

   % Compute the quadrature node:

   q=(1/3)*sum(c);

   % Evaluate the integrand at the quadrature node:

   if isnumeric(fnk)
      I=fnk;
   else
      I=feval(fnk,q(1),q(2));
   end

   if ~isnumeric(p1)

      I=I*(feval(p1,q(1),q(2))^2+feval(p2,q(1),q(2))^2);

   else

      % Each basis function, restricted to this triangle, is a function
      % of the form z=g(1)+g(2)x+g(3)y.  Compute the vector g for each of
      % three basis functions (the three vectors are stored as the columns
      % of the matrix C).

      M=[vo,c];
      C=inv(M);

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

      % The gradients are constant vectors, since the basis functions
      % are linear over this triangle.   So we just compute all the
      % dot products and store them in a matrix G:

      G=C(2:3,:)'*C(2:3,:);

      I=I*(v'*(G*v));

   end

   % Finally, add the contribution to the integral:

   m=m+A*I;

end

m=sqrt(m);

