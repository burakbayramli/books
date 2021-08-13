function K=Stiffness1(T,fnk)

% K=Stiffness1(T,fnk)
%
%   Assembles the stiffness matrix for the PDE
%
%        -div(k*grad u)=f in Omega,
%             u=0 on Gamma,
%             du/dn=0 on Bndy(Omega)-Gamma.
%
%   The input fnk can be a (positive) number k or a function
%   implementing a function k(x,y).  If fnk is omitted, it is
%   assumed to be the constant 1.  T describes the triangulation
%   of Omega.  For a description of the data structure T, see
%   "help Mesh1".

if nargin<2
   fnk=1;
end

% Allocate the stiffness matrix:

N=length(T.FNodePtrs);
K=sparse(N,N);

% Loop over the elements, adding the contributions from each.

vo=ones(3,1);
for i=1:size(T.ElList,1)

   % Get the indices of the vertices of this triangle:

   kk=T.ElList(i,1:3);

   % Get the coordinates of the vertices:

   c=T.NodeList(kk,1:2);

   % Get the indices of the nodes in the list of free nodes:

   ll=T.NodePtrs(kk);

   % Each basis function, restricted to this triangle, is a function
   % of the form z=g(1)+g(2)x+g(3)y.  Compute the vector g for each of
   % three basis functions (the three vectors are stored as the columns
   % of the matrix C).

   M=[vo,c];
   C=inv(M);

   % The typical integral we must compute is
   %
   %      integral over Ti (k(x,y)(grad phi1).(grad phi2).
   %
   % The gradients are constant vectors, since the basis functions
   % are linear over this triangle.   So we just compute all the
   % dot products and store them in a matrix G:

   G=C(2:3,:)'*C(2:3,:);

   % Compute the area of the triangle:

   J=[c(2,1)-c(1,1) c(3,1)-c(1,1);c(2,2)-c(1,2) c(3,2)-c(1,2)];
   A=0.5*abs(det(J));

   % Apply the quadrature rule:

   if isnumeric(fnk)
      I=fnk*A;
   else
      % Compute the centroid of the triangle:

      qpt=(1/3)*sum(c);

      I=A*feval(fnk,qpt(1),qpt(2));
   end

   % The triangle contributes to at most 6 entries in the (upper triangle
   % of the) stiffness matrix.  We compute these six entries in the
   % following double loop.

   for s=1:3

      lls=ll(s);
      if lls>0

         for r=1:s

            % If both vertices are free, then there is a contribution
            % to the stiffness matrix

            llr=ll(r);
            if llr>0

               if llr<=lls
                  K(llr,lls)=K(llr,lls)+G(r,s)*I;
               else
                  K(lls,llr)=K(lls,llr)+G(r,s)*I;
               end

            end

         end

      end

   end

end

% Now fill in the lower triangle of K, using the symmetry.

K=K + triu(K,1)';
