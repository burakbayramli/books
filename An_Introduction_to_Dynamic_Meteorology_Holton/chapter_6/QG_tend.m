function qgtend = QG_tend(t,xin,varargin)

% calculate righthand side of tendency equations for PV and boundary potential temperature
% 
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-09 09:31:05 -0800 (Thu, 09 Feb 2012) $
% $Revision: 788 $
% $Author: hakim $
% $Id: QG_tend.m 788 2012-02-09 17:31:05Z hakim $
  
global Nx Ny Nz DX DY facx facy dz z

% initialize varargin variables
lambda=0; THY_b=zeros(Ny,Nx); THY_t=zeros(Ny,Nx); Ubar_b=zeros(Ny,Nx); Ubar_t=zeros(Ny,Nx); 
Ubar = zeros(Nz,Ny,Nx); Unot = 0;

% determine the number of fixed and variable arguments passed to the function
nargin;
nfixedargs = abs(nargin('QG_tend')) - 1; % -1 for varargin 
nvarargin = nargin - nfixedargs;
if mod(nvarargin,2) ~= 0
  error('must pass argument-value pairs')
end
a = {};
if isempty(varargin) == 0
  a = varargin;
  for k = 1:2:nvarargin
	 varn = varargin{k};
	 varv = varargin{k+1};
	 if isequal(varn,'lambda') 
		lambda = varv;
	 elseif isequal(varn,'THY_b') % spectral!
		THY_b = varv;
	 elseif isequal(varn,'THY_t') % spectral!
		THY_t = varv;
	 elseif isequal(varn,'Ubar_b') % spectral!
		Ubar_b = varv;
	 elseif isequal(varn,'Ubar_t') % spectral!
		Ubar_t = varv;
	 elseif isequal(varn,'Ubar') % spectral!
		Ubar = varv;
	 elseif isequal(varn,'Unot')
		Unot = varv;
	 end
  end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% linear shear and/or constant wind 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% non-dimensional linear shear STAGGERED GRID
Ub = lambda * z + Unot;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extract pv, theta_b, and theta_t from the column vector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pv = reshape(xin(1:Nx*Ny*Nz),Nz,Ny,Nx);
lb = (Nx*Ny*Nz)+1;
theta_b = reshape(xin(lb:lb+(Nx*Ny)-1),Ny,Nx);
lb = lb+(Nx*Ny);
theta_t = reshape(xin(lb:end),Ny,Nx);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% call generic solver (phixy is on staggered grid)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idn = 1; % Nuemann BC 
[phixy,phibxy,phitxy,thetabxy,thetatxy] = inv_laplacian(theta_b,theta_t,pv,idn,dz,facx,facy);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pv tendency equation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for k = 1:1:Nz
  phisp = fft2(squeeze(phixy(k,:,:)));
  pvsp = fft2(squeeze(pv(k,:,:)));
  dpvdx = i*DX.*pvsp;
  dpvdy = i*DY.*pvsp;
  usp = -i*DY.*phisp; 
  % add linear shear to mean at this level:
  usp(1,1) = usp(1,1) + Ub(k)*Nx*Ny;
  % add periodic part of basic-state shear
  usp = usp + squeeze(Ubar(k,:,:));
  vsp =  i*DX.*phisp;
  uadv = QG_nonlin(usp,dpvdx);
  vadv = QG_nonlin(vsp,dpvdy); 
  pvadvsp  = -(uadv + vadv);
  pvadvsp(:,Nx/2) = 0; pvadvsp(Ny/2,:) = 0; % eliminate Nyquist wavenumbers
  pvtend(k,:,:)  = real(ifft2(pvadvsp));
  % option for a figure during testing
  if 1 == 0
	 figure(1); clf
	 contour(real(ifft2(pvadvsp))); colorbar
	 pause
  end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lower-boundary tendency equation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
phisp = fft2(phibxy);
thetasp = fft2(theta_b);
dtdx = i*DX.*thetasp;
dtdy = i*DY.*thetasp;
% add linear shear to mean at this level:
dtdy(1,1) = dtdy(1,1) - lambda*Nx*Ny; 
% add periodic part of basic state 
dtdy = dtdy + THY_b;
usp = -i*DY.*phisp;
% add linear shear to mean at this level:
usp(1,1) = usp(1,1) + Unot*Nx*Ny;
% add periodic part of basic state 
usp = usp + Ubar_b;
vsp =  i*DX.*phisp;
uadv = QG_nonlin(usp,dtdx);
vadv = QG_nonlin(vsp,dtdy); 
tadvsp = -(uadv + vadv);
tadvsp(:,Nx/2) = 0; tadvsp(Ny/2,:) = 0; % eliminate Nyquist wavenumbers
tbtend = real(ifft2(tadvsp));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% upper-boundary tendency equation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
phisp = fft2(phitxy);
thetasp = fft2(theta_t);
dtdx = i*DX.*thetasp;
dtdy = i*DY.*thetasp;
% add linear shear to mean at this level:
dtdy(1,1) = dtdy(1,1) - lambda*Nx*Ny; 
% add periodic part of basic state 
dtdy = dtdy + THY_t;
usp = -i*DY.*phisp;
% add linear shear to mean at this level:
usp(1,1) = usp(1,1) + (lambda+Unot)*Nx*Ny;
% add periodic part of basic state 
usp = usp + Ubar_t;
vsp =  i*DX.*phisp;
uadv = QG_nonlin(usp,dtdx);
vadv = QG_nonlin(vsp,dtdy); 
tadvsp = -(uadv + vadv);
tadvsp(:,Nx/2) = 0; tadvsp(Ny/2,:) = 0; % eliminate Nyquist wavenumbers
tttend = real(ifft2(tadvsp));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% state tendency column vector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pvtvec = reshape(pvtend,Nx*Ny*Nz,1);
tbtvec = reshape(tbtend,Nx*Ny,1);
tttvec = reshape(tttend,Nx*Ny,1);
qgtend = [pvtvec; tbtvec; tttvec];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% option to evaluate courant numbers for calibrating dt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if 1 == 0 
  cx = max(max(abs(ifft2(usp)))) * dt / (Lx/Nx);
  disp(['x Courant number = ' num2str(cx)]);
end

