% Example1
%
%   This script applies the Fem1 routines to solve the BVP given below,
%   graph the mesh and the solution, and compute the error in the solution.
%
%   The BVP is
%
%          -lap u=(5pi^2)sin(pi x)sin(pi y) in Omega
%               u=0 on Bndy Omega
%
%   Omega is the unit square, and the exact solution is
%   u(x,y)=sin(pi x)sin(2pi y).

% Create the problem functions

fnf=@(x,y)5*pi^2*sin(pi*x).*sin(2*pi*y);
fnu=@(x,y)sin(pi*x).*sin(2*pi*y);
fnux=@(x,y)pi*cos(pi*x).*sin(2*pi*y);
fnuy=@(x,y)2*pi*sin(pi*x).*cos(2*pi*y);

% Create a mesh and display it:

T=RectangleMeshD1(10);
figure(1);
clf
ShowMesh1(T)

% Compute the stiffness matrix and load vector:

K=Stiffness1(T);
F=Load1(T,fnf);

% Solve for the nodal values:

U=K\F;

% Display the solution

figure(2);
clf
ShowPWLinFcn1(T,U)

% Next we illustrate the convergence of the error to zero
% as the mesh is refined.  We will compute the errors in both
% the L2 and energy norms.  The results show that the L2 error
% decreases approximately as h^2, while the energy norm error
% decreases approximately as h.

% Now compute the L2 and energy norm errors of the solution computed
% on successively finer meshes:

l2normerr=zeros(5,1);
enormerr=zeros(5,1);

disp('   Refinement  Energy Error   L2 Error')
for n=1:5

   T=RectangleMeshD1(2^n);
   K=Stiffness1(T,1);
   F=Load1(T,fnf);
   U=K\F;
   enormerr(n)=EnergyNormErr1(T,1,fnux,fnuy,U);
   l2normerr(n)=L2NormErr1(T,fnu,U);
   disp([n l2normerr(n) enormerr(n)])

end

% Compute the L2 norm of the exact solution on the finest mesh:

l2normsol=L2Norm1(T,fnu);

% Compute the energy norm of the exact solution on the finest mesh:

enormsol=EnergyNorm1(T,1,fnux,fnuy);
