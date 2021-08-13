function m=EnergyNormErr1(T,fnk,fnux,fnuy,U,g)

% m=EnergyNormErr1(T,fnk,fnux,fnuy,U,g)
%
%   This function computes the energy norm of the difference between
%   the smooth function u(x,y) and the piecewise linear function defined by
%   the mesh T and the nodal values U.  The function fnk is the weight
%   in the energy inner product, while fnux and fnuy are the partial
%   derivatives of u(x,y).  fnk can be a (positive) number or a real-valued
%   function of two variables, while fnux and fnuy must be real-valued
%   functions of two real variables.
%
%   The optional input g is a vector containing the nodal values of the
%   piecewise linear function at the constrained nodes.  If g is not given,
%   these values are taken to be zero.
%
%   See "help Mesh1" for a description of the mesh T.

if nargin<6
   g=zeros(size(T.CNodePtrs));
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

   % Each basis function, restricted to this triangle, is a function
   % of the form z=g(1)+g(2)x+g(3)y.  Compute the vector g for each of
   % three basis functions (the three vectors are stored as the columns
   % of the matrix C).

   M=[vo,c];
   C=inv(M);

   % Compute the gradient of the linear function on this triangle.
   % The coordinates of the vertices are contained in c.

   gv=C(2:3,:)*v;

   % Compute the gradient of u(x,y) at the quadrature node:

   gu=[feval(fnux,q(1),q(2));feval(fnuy,q(1),q(2))];

   % Evaluate the integrand at the quadrature node:

   if isnumeric(fnk)
      I=fnk;
   else
      I=feval(fnk,q(1),q(2));
   end
   I=I*norm(gv-gu)^2;

   % Now add the contribution to the integral

   m=m+A*I;

end

m=sqrt(m);

