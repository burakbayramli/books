function F=Load1(T,fnf)

% F=Load1(T,fnf)
%
%   Assembles the load vector for the BVP
%
%        -div(a*grad u)=f in Omega,
%             u=0 on Gamma,
%             du/dn=0 on Bndy(Omega)-Gamma.
%
%   The right hand side function f(x,y) must be implemented in the
%   function fnf.

% Allocate the load vector F:

N=length(T.FNodePtrs);
F=zeros(N,1);

% Add the contribution from each element

vo=ones(3,1);
for i=1:size(T.ElList,1)

   % Get the indices of the vertices of this triangle:

   kk=T.ElList(i,1:3);

   % Get the coordinates of the vertices:

   c=T.NodeList(kk,1:2);

   % Get the indices of the nodes in the list of free nodes:

   ll=T.NodePtrs(kk);

   % The lone quadrature point is the centroid of the triangle:

   qpt=(1/3)*sum(c);

   % Compute the area of the triangle:

   J=[c(2,1)-c(1,1) c(3,1)-c(1,1);c(2,2)-c(1,2) c(3,2)-c(1,2)];
   A=0.5*abs(det(J));

   % Get the value of fnf at the quadrature node:

   fval=feval(fnf,qpt(1),qpt(2));

   % The triangle contributes to at most 3 entries in the load vector.
   % All contributions are the same (all three basis functions have
   % value 1/3 at the quadrature node):

   I=(A*fval)/3;

   for r=1:3

      % If this vertex is free, then there is a contribution to the
      % load vector:

      llr=ll(r);
      if llr>0
         F(llr)=F(llr)+I;
      end

   end

end
