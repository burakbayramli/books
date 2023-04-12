%----------------------------------------------------
% Program: diffusion2d.m
% 2D FEM - diffusion equation
% Discretisation with 4-node quadrilaterals
%----------------------------------------------------
clear
seconds_per_yr = 60*60*24*365 ; % seconds in 1 year
% physical parameters
kappa = 1e-6 ; % thermal diffusivity, m^2/s
lx = 1e3*2
; % width of domain, m
ly = 1e3
; % depth of domain, m
H = 0*1e-9 ; % heat source, o K/s
Tb = 0 ;
% fixed boundary temperature
Ti = 1 ;
% initial temperature
% numerical parameters
ndim = 2;
% number of spatial dimensions
nod = 4;
% number of nodes per element
nelx = 50;
% number of elements in x-direction
nely = 50;
% number of elements in y-direction
nels = nelx*nely ; % total number of elements
nx= nelx+1 ;
% number of nodes in x-direction
ny= nely+1 ;
% number of nodes in y-direction
nn= nx*ny ;
% total number of nodes
nip= 4;
% number of integration points
dx= lx/nelx ;
% element length in x-direction
dy= ly/nely ;
% element length in y-direction
dt= 10*seconds_per_yr ; % time step (s)
ntime = 60 ;
% number of time steps to perform
kay= eye(ndim)*kappa ; % thermal diffusivity tensor
% define mesh (numbering in y direction)
g_coord = zeros(ndim,nn) ;
n = 1 ;

for i=1:nx
  for j=1:ny
    g_coord(1,n) = (i-1)*dx ;
    g_coord(2,n) = (j-1)*dy ;
    n = n + 1 ;
  end
end

% establish elem-node connectivity
gnumbers = reshape(1:nn,[ny nx]) ;
iel = 1 ;
for i=1:nelx
  for j=1:nely
    g_num(1,iel) = gnumbers(j,i) ;
    g_num(2,iel) = gnumbers(j+1,i) ;
    g_num(3,iel) = gnumbers(j+1,i+1);
    g_num(4,iel) = gnumbers(j,i+1);
    iel = iel + 1 ;
  end
end

% find boundary nodes
eps = 0.01*min(dx,dy) ;
% small fraction of dx or dy
bx0 = find(g_coord(1,:) <= 0+eps) ; % nodes on x=0 boundary
bxn = find(g_coord(1,:) >= lx-eps) ; % nodes on x=lx boundary
by0 = find(g_coord(2,:) <= 0+eps) ; % nodes on y=0 boundary
byn = find(g_coord(2,:) >= ly-eps) ; % nodes on y=ly boundary
% define boundary conditions
bcdof = unique([bx0 bxn by0 byn]) ; % boundary nodes
bcval = Tb*ones(1,length(bcdof)) ; % boundary temperatures
% gauss integration data
points = zeros(nip,ndim); % location of points
root3 = 1./sqrt(3);
points(1,1)=-root3; points(1,2)= root3;
points(2,1)= root3; points(2,2)= root3;
points(3,1)=-root3; points(3,2)=-root3;
points(4,1)= root3; points(4,2)=-root3;
wts = ones(1,4) ; % weights
% save shape functions and their derivatives in local coordinates
% evaluated at integration points
for k=1:nip
xi = points(k,1) ;
eta = points(k,2) ;
etam = 0.25*(1-eta); etap = 0.25*(1+eta) ;
xim = 0.25*(1-xi) ; xip = 0.25*(1+xi) ;
fun = 4*[xim*etam xim*etap xip*etap xip*etam ] ;
fun_s(k,:) = fun ; % shape functions
der(1,1)=-etam; der(1,2)=-etap; der(1,3)=etap; der(1,4)=etam ;
der(2,1)=-xim; der(2,2)=xim;
der(2,3)=xip; der(2,4)=-xip ;
der_s(:,:,k) = der ; % derivatives of shape function
end
% initialise arrays
ff = zeros(nn,1);
b = zeros(nn,1);
lhs = sparse(nn,nn);
rhs = sparse(nn,nn);

% x and y grids for plotting
xgrid = reshape(g_coord(1,:),ny,nx) ;
ygrid = reshape(g_coord(2,:),ny,nx) ;
%----------------------------------------------------
% matrix integration and assembly
%----------------------------------------------------
for iel=1:nels % sum over elements
  num = g_num(:,iel)
  coord = g_coord(:,num)' ; % element coordinates
  KM = zeros(nod,nod);
  MM = zeros(nod,nod);
  F = zeros(nod,1);
  for k = 1:nip % integration loop
    fun = fun_s(k,:) ; % shape functions
    der = der_s(:,:,k) ; % der. of shape functions in local coordinates
    jac = der*coord ;
    detjac = det(jac) ;
    invjac = inv(jac) ;
    deriv = invjac*der ;
    KM = KM + deriv'*kay*deriv*detjac*wts(k) ; % stiffness matrix
    MM = MM + fun'*fun*detjac*wts(k) ;
    F = F + fun'*H*detjac*wts(k) ;
% load vector
end
% assemble global matrices and vector
lhs(num,num) = lhs(num,num) + MM/dt + KM ;
rhs(num,num) = rhs(num,num) + MM/dt;
ff(num) = ff(num) + F;
end
%----------------------------------------------------
% time loop
%----------------------------------------------------
displ = Ti*ones(nn,1) ;% initial conditions
time = 0 ; % initialise time
for n=1:ntime
  time = time + dt ;
  b = rhs*displ + ff;
  lhs(bcdof,:) = 0 ;
  tmp = spdiags(lhs,0) ;
  tmp(bcdof)=1 ;
  lhs=spdiags(tmp,0,lhs);
  b(bcdof) = bcval ; %
  displ = lhs \ b ; % solve system of equations

%----------------------------------------------------
% exact solution from Carslaw and Jaeger (1959)
% page 173, eqns 6 and 11
nterms = 100 ;
t=time; x=xgrid-lx/2; y=ygrid-ly/2 ;
l = lx/2 ; b = ly/2;
sumx = 0 ; sumy = 0 ;
for m=0:nterms
  eterm = exp(-kappa*(2*m+1)^2*pi^2*t/(4*l^2));
  costerm = cos((2*m+1)*pi*x/(2*l)) ;
  sumx = sumx + (-1)^m/(2*m+1)*eterm*costerm ;
  eterm = exp(-kappa*(2*m+1)^2*pi^2*t/(4*b^2));
  costerm = cos((2*m+1)*pi*y/(2*b)) ;
  sumy = sumy + (-1)^m/(2*m+1)*eterm*costerm ;
end
psix = 4/pi*sumx ;
psiy = 4/pi*sumy ;
exact = psix.*psiy ;
%----------------------------------------------------
% plot numerical and exact solutions
figure(1)
solution = reshape(displ,ny,nx) ;
isotherms = [0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9] ;
contour(xgrid,ygrid,solution,isotherms,'b.')
hold on

contour(xgrid,ygrid,exact,isotherms,'r')
xlabel('x-distance (m)')
ylabel('y-distance (m)')
title('Temperature isotherms')
axis equal
hold off
drawnow
end
