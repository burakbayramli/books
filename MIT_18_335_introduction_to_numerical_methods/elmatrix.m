function K=elmatrix(h,material)
%ELMATRIX Create linear elastic elemental stiffness matrix on 4-node quad.
%   K=ELMATRIX(H,MATERIAL) returns the 8-by-8 elemental matrix associated
%   with the finite element discretization of the 2-D linear elastic
%   Navier's equations on a square with side length H. The material is
%   described by the vector MATERIAL=[E,NU,TYPE], where E is Young's
%   modulus, NU is Poisson's ratio, and TYPE is 0 for plane stress analysis
%   or 1 for plane strain analysis.
%
%   The eight degrees of freedom are arranged as [u1,u2,u3,u4,v1,v2,v3,v4],
%   where u is the x-displacement, v is the y-displacement, and the numbers
%   1,2,3,4 correspond to the SW,SE,NW,NE corners.
%
%   K=ELMATRIX(H) uses the default MATERIAL=[1,1/3,0].
%
%   Example (all integer matrix, plot eigenfunctions):
%      K=round(elmatrix(1,[32,1/3,0]))
%      rank(K) % Verify 3 null vectors (translation x,y + rotation)
%      [V,D]=eig(K);
%      for ii=1:size(V,2)
%        disp(sprintf('lambda_%d = %g',ii,D(ii,ii)))
%        qdplot(V(:,ii))
%        pause
%      end
%
%   See also: ASSEMBLE, MKMODEL, QDPLOT.

%   Per-Olof Persson <persson@math.mit.edu>

if nargin<2, material=[1,1/3,0]; end

E=material(1); nu=material(2); analysis=material(3);

if analysis==0 % Plane stress
  C=E/(1-nu^2)*[1,nu,0;nu,1,0;0,0,(1-nu)/2];
else % Plane strain
  C=E*(1-nu)/(1+nu)/(1-2*nu)* ...
    [1,nu/(1-nu),0;nu/(1-nu),1,0;0,0,(1-2*nu)/2/(1-nu)];
end

gp=[1,1;1,-1;-1,1;-1,-1]/sqrt(3);
gw=[1;1;1;1]/4;

K=zeros(8,8);
for igp=1:size(gp,1)
  x=gp(igp,1)/2; y=gp(igp,2)/2; w=gw(igp);
  B=[-(1-y),(1-y),-(1+y),(1+y),0,0,0,0;
     0,0,0,0,-(1-x),-(1+x),(1-x),(1+x);
     -(1-x),-(1+x),(1-x),(1+x),-(1-y),(1-y),-(1+y),(1+y)]/2/h;
  K=K+B'*C*B*w*h^2;
end
