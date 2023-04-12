%---------------------------------------------
% Program diffn1d.m
% 1-D FEM solution of diffusion equation
% and comparison with analytical solution
%---------------------------------------------
clear % clear memory from current workspace
seconds_per_yr = 60*60*24*365; % number of seconds in 1 year
% physical parameters
lx= 2000 ;
% length of spatial domain
Cp= 1e3 ;
% rock heat capacity J/kg/K
rho= 2700 ;
% rock density
K= 3.3 ;
% bulk thermal conductivity W/m/K
kappa = K/(Cp*rho); % thermal diffusivity
tau= 10e6 ;
% shear stress resolved on fault (Pa)
udot = 10e-3/seconds_per_yr ;
% fault slip rate (m/s)
dTdx = (1/2)*tau*udot/(rho*Cp*kappa) ; % T gradient at fault

% numerical parameters
dt= seconds_per_yr ;
theta= 1
ntime= 5000 ;
nels= 200 ;
nod= 2 ;
nn= nels+1
dx= lx/nels ;
g_coord = [0:dx:lx] ;
% explicit time stepping options
lumped_explicit = 'N';
if theta==0 % if fully explicit
lumped_explicit = input('Would you like to lump the mass matrix? Y/N [N]:','s');
if isempty(lumped_explicit)
lumped_explicit = 'N';
end
end


%
bcdof = [ nn ] ; % boundary nodes
bcval = [ 0 ]
; % boundary values
% define connectivity and equation numbering
g_num= zeros(nod,nels) ;
g_num(1,:) = [1:nn-1] ;
g_num(2,:) = [2:nn]
;
% initialise matrices and vectors
b= zeros(nn,1);
% system rhs vector
lhs= sparse(nn,nn);
% system lhs matrix
rhs= sparse(nn,nn);
% system rhs matrix
displ = zeros(nn,1);
% initial temperature (o C)
lumped_diag = zeros(nn,1) ;
% storage for lumped diagonal
%---------------------------------------------
% matrix assembly
%---------------------------------------------
for iel=1:nels % loop over all elements
num = g_num(:,iel) ;
% retrieve equation number
dx = abs(diff(g_coord(num))) ; % length of element
MM = dx*[1/3 1/6 ; 1/6 1/3 ] ;% mass matrix
KM = [kappa/dx -kappa/dx ; -kappa/dx kappa/dx ];% diffn matrix
if lumped_explicit=='N'
lhs(num,num) = lhs(num,num) + MM/dt + theta*KM ; % assemble lhs
rhs(num,num) = rhs(num,num) + MM/dt - (1-theta)*KM ; % assemble rhs
else
lumped_diag(num) = lumped_diag(num) + sum(MM)'/dt ; % lumped diagonal
rhs(num,num) = rhs(num,num) + diag(sum(MM))/dt - (1-theta)*KM ; % assemble rhs
end
end
% end of element loop
%---------------------------------------------
% time loop
t = 0 ; % time
k = 1 ; % counter
ii = [100 1000 5000]; % array used for plotting
for n=1:ntime
  n
  t = t + dt ;
  b = rhs*displ ;
  lhs(bcdof,:) = 0 ;
  tmp = spdiags(lhs,0) ;
  tmp(bcdof)=1 ;
  lhs=spdiags(tmp,0,lhs);
  b(bcdof) = bcval ; %
  b(1) = b(1) + dTdx*kappa ; % add heat flux at left boundary
  if lumped_explicit=='N'
    displ = lhs \ b ; % solve system of equations
  else
    displ = b./lumped_diag ; % fully explicit, diagonalised solution
  end

  x = g_coord ;
  term1 = abs(x).*sqrt(pi).*erf((1/2)*abs(x)./(sqrt(kappa./t).*t));
  term2 = 2*t.*exp(-(1/4)*abs(x).^2./(kappa*t)).*sqrt(kappa./t);
  term3 = abs(x)*sqrt(pi) ;
  term4 = (tau*udot)/(2*kappa*sqrt(pi)*rho*Cp);
  Texact = term4*(term1+term2-term3);

  if mod(n,ii(k))==0
    k = k+1;
    hold on
    figure(1)
    plot(g_coord,displ,'o-',g_coord,Texact,'r')
    title(['Time (kyr) = ', num2str(t/seconds_per_yr/1e3)])
    xlabel('Distance away from fault (m)')
    ylabel('Temperature (o C)')
    drawnow
    t/seconds_per_yr
    pause
  end
  hold off
end % end of time loop  
