%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%======================================================================
% Exact solution for the shock tube problem
% Input arguments:
%      x      vector of dimension n containing the space discretization
%     x0      initial position x_0 of the diaphragm
%      t      time at which the solution is computed 
% Output arguments:
%      uex  vector of dimension (3,n) containing the exact solution 
%           uex(1,1:n)  the density rho
%           uex(2,1:n)  the velocity U
%           uex(3,1:n)  the pressure p
%======================================================================

function uex=HYP_shock_tube_exact(x,x0,t)
  
  global gamma aL pL rhoL aR pR rhoR dum1 dum2 dum3;

  M=length(x);
  uex=zeros(3,M); % initialization
  
			  % Regions with constant parameters 
			  %===========================================
  
  
		    %----- computation of M_s (compatibility relation)
  Ms=fzero('HYP_mach_compat',2.);
  fprintf('Shock Mach number Ms=%f \n',Ms);
  
			    %----- region (L) (rhoL,pL, aL are given) 
			    %----- region (1)
  dumm=Ms*Ms;
  
  p1  = pR*(dum1*gamma*dumm-dum2);
  rho1= rhoR/(dum1/dumm+dum2);
  U1  = dum1*(Ms-1./Ms);
  a1  = sqrt(gamma*p1/rho1);
  
				%----- region (2) (U2=U1, p2=p1)
  a2  = aL-dum3*U1;
  rho2= rhoL*(p1/pL)^(1./gamma);
  
				%----- region (R) (UR=pR=aR=1)
  
		% Separation of regions (abscissas x_1, x_2, x_3, x_4)
		%====================================================
  x1  = x0-aL*t;
  x2  = x0+(U1-a2)*t;
  x3  = x0+U1*t;
  x4  = x0+Ms*t;
  
		 % Exact solution (density, velocity, pressure)
		 %====================================================
		 %----- region (L)
  idum = find(x<=x1);
  uex(1,idum) = rhoL;
  uex(2,idum) = 0.;
  uex(3,idum) = pL;
  
				%----- region (E)
  idum = find((x1<x)&(x<=x2));
  uex(2,idum) = dum1*(aL+     (x(idum)-x0)/t);
  adet = dum1*(aL-dum3*(x(idum)-x0)/t);
  uex(3,idum) = pL*(adet/aL).^(2*gamma/(gamma-1));
  uex(1,idum) =gamma*uex(3,idum)./(adet.*adet);
  
				%----- region (2)
  idum = find((x2<x)&(x<=x3));
  uex(1,idum) = rho2;
  uex(2,idum) = U1;
  uex(3,idum) = p1;
  
				%----- region (1)
  idum = find((x3<x)&(x<=x4));
  uex(1,idum) = rho1;
  uex(2,idum) = U1;
  uex(3,idum) = p1;
  
				%----- zone (R)
  idum = find(x4<x);
  uex(1,idum) = 1;
  uex(2,idum) = 0;
  uex(3,idum) = 1/gamma;
  
  
  
  
