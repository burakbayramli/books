
function [fxy,fbxy,ftxy,fzbxy,fztxy] = inv_laplacian(lbcxy,ubcxy,qxy,idn,dz,facx,facy)
  
%
% invert f_xx + f_yy + f_zz = q(x,y,z) given either Neumann or Dirichlet BCs
%
% f must be periodic in (x,y).
%
% idn =  1: Neumann BC
% idn = -1: Dirichlet BC
% 
% Cartesian variables suffix 'xy'
% Spectral variables suffix 'sp'  
%
% returns f: interior field on 'staggered' z grid
%         fb: lower boundary
%         ft: upper boundary
%         fzb: df/dz on lower bounary
%         fzt: df/dz on upper bounary
%
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-07 06:36:37 -0800 (Tue, 07 Feb 2012) $
% $Revision: 783 $
% $Author: hakim $
% $Id: inv_laplacian.m 783 2012-02-07 14:36:37Z hakim $
  
nlevs = size(qxy,1);
Ny = size(qxy,2); lmax = Ny/2;
Nx = size(qxy,3); kmax = Nx/2;

% map RHS and boundary conditions to Fourier space
lbcsp = fft2(lbcxy); ubcsp = fft2(ubcxy);
for k = 1:1:nlevs
  qsp(k,:,:) = fft2(squeeze(qxy(k,:,:)));
end

% factors for tanh vertical grid
%[facz,faczo,zl,zhl] = setup_tanh(nlevs);
% no tanh grid yet; these allow below to work
facz = ones(1,nlevs); faczo = facz;

% add boundary condition to a copy of spectral RHS:
if idn == 1 % Neumann boundary condition
  qsp(1,:,:) = squeeze(qsp(1,:,:)) + (lbcsp/(facz(1)*dz));
  qsp(nlevs,:,:) = squeeze(qsp(nlevs,:,:)) - (ubcsp/(facz(nlevs)*dz));
elseif idn == -1 % Dirichlet boundary condition
  qsp(1,:,:) = squeeze(qsp(1,:,:)) - (2*lbcsp/(facz(1)*facz(1)*dz*dz));
  qsp(nlevs,:,:) = squeeze(qsp(nlevs,:,:)) - (2*ubcsp/(facz(nlevs)*facz(nlevs)*dz*dz));
else
  error('wrong specification of idn')
end

% tridiagonal matrix inversion for each (k,l)
fsp = zeros(nlevs,Ny,Nx);
%tic
for k=1:1:2*kmax; 
  for l=1:1:2*lmax;
	 [ak,bl] = get_waves(k,l,kmax,lmax,facx,facy);
	 if ((k == 1) & (l == 1)) % no information on (x,y) mean
		fbsp(l,k) = 0.; ftsp(l,k) = 0.; fsp(:,l,k) = 0.;
     else
		 % construct matrix for inversion
		 dd = diag(-1.*((ak*ak) + (bl*bl) + (2./(dz*dz)))*ones(nlevs,1));
		 ud = diag((1/(dz*dz))*ones(nlevs-1,1),1);
		 ld = diag((1/(dz*dz))*ones(nlevs-1,1),-1);
		 A = dd + ud + ld;
		 % fix upper left and lower right corners from BCs
		 if idn == 1
			A(1,1) = -1.*((ak*ak) + (bl*bl) + (1./(dz*dz)));
		 elseif idn == -1
			A(1,1) = -1.*((ak*ak) + (bl*bl) + (3/(dz*dz)));			
		 end
		 A(end,end) = A(1,1);
		 % apparently this is optimized in newer versions of Matlab
		 psi = A\qsp(:,l,k);
		 % alternative implementation of the Thomas algorithm
		 %psi = matinv(nlevs,bpvsp(:,l,k),ak,bl,idn,dz);
		 fsp(:,l,k) = psi(:);
		 if idn == 1 % Neumann boundary conditions
			fbsp(l,k) = psi(1) - (0.5*dz*facz(1)*lbcsp(l,k));
			ftsp(l,k) = psi(nlevs) + (0.5*dz*facz(nlevs)*ubcsp(l,k));
			fzbsp(l,k) = lbcsp(l,k);
			fztsp(l,k) = ubcsp(l,k);
		 elseif idn == -1
			fzbsp(l,k) = 2*(psi(1) - lbcsp(l,k))/(dz*facz(1));
			fztsp(l,k) = 2*(ubcsp(l,k) - psi(nlevs))/(dz*facz(nlevs));
			fbsp(l,k) = lbcsp(l,k);
			ftsp(l,k) = ubcsp(l,k);
		 end
	 end
  end
end  
%toc

% map back to grid 
fbxy = real(ifft2(fbsp));
ftxy = real(ifft2(ftsp));
fzbxy = real(ifft2(fzbsp));
fztxy = real(ifft2(fztsp));

for k = 1:1:nlevs
  fxy(k,:,:) = real(ifft2(squeeze(fsp(k,:,:))));
end
