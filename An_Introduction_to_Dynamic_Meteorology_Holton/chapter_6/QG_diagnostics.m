%
% solve QG diagnostic equations
%
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-10 12:09:59 -0800 (Fri, 10 Feb 2012) $
% $Revision: 790 $
% $Author: hakim $
% $Id: QG_diagnostics.m 790 2012-02-10 20:09:59Z hakim $

clear

% menu options (1 turns on; 0 turns off)
idata     = 1; % 1 = data created below by QG_initial_value; 2 = QG model output
iplot     = 1; % plots
tendsolve = 1; % height tendency
wsolve    = 1; % omega equation
ageosolve = 1; % ageostrophic wind

% set name of datafile and select a time (for idata = 2 ONLY)
datafile = 'QGout_20120208.115623.mat'; 
it = 2; % select a time slice here

% initialize computational grid, scaling parameters, and vertical reference state
grid_setup;
scaling;
reference_state;

% initialize figure number
fign = 0;

if idata == 1
  % specify a PV distribution (pvmag in PVU; thmag in K)
  ipv = 4; pvmag = 2e-6; thmag = 0; % interior PV blob with no boundary theta
  %ipv = 5; pvmag = 1.5e-6; thmag = 0; % interior PV dipole with no boundary theta
  %ipv = 6; pvmag = 0; thmag = 10; % surface theta only
  %ipv = 61; pvmag = 0; thmag = -10; % tropopause theta only
  QG_initial_value;
  % set non-dimensional linear vertical shear 
  %lambda = 3; % shear parameter
  lambda = 0; % shear parameter
  Unot = 0;   % constant wind
elseif idata == 2
  load(datafile)
  xin = Xf(it,:);
  pvxy = reshape(xin(1:Nx*Ny*Nz),Nz,Ny,Nx);
  lb = (Nx*Ny*Nz)+1;
  lbcxy = reshape(xin(lb:lb+(Nx*Ny)-1),Ny,Nx);
  lb = lb+(Nx*Ny);
  ubcxy = reshape(xin(lb:end),Ny,Nx);
  if it < 0 | it > length(tspan)
	 error(['time out of bounds. it=' int2str(it) ' number of times=' int2str(length(tspan))])
  else
	 disp(['time selected is ' num2str(tspan(it)*Tstar/3600) ' hours'])
	 %disp(['time selected is ' num2str(tspan(it)*Tstar/86400) ' days'])  
  end
else
  error('idata must be 1 or 2')
end

% basic-state wind
Ub = lambda * z + Unot; % staggered grid
Ubu = lambda * zu + Unot; % unstaggered grid

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% invert PV/theta for pressure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

idn = 1; % Nuemann BC 
pvxy = pvxy - mean(mean(mean(pvxy)))*XL*YL*ZH;

% call generic solver (phixy is on staggered grid)
[phixy,phibxy,phitxy,thetabxy,thetatxy] = inv_laplacian(lbcxy,ubcxy,pvxy,idn,dz,facx,facy);

% check the PV by direct calculation (using spectral fields)
% need these fields for later calculations anyway!
for k = 1:1:pmax
  phisp(k,:,:) = fft2(squeeze(phixy(k,:,:)));
end
tbsp = fft2(thetabxy); ttsp = fft2(thetatxy);
pvcheck = laplacian(nlevs,phisp,tbsp,ttsp,DX,DY,dz);

% process and check at one level (may have different means, which is anirrelevant constant)
ilz = kmid; 
mpv = mean(mean(pvxy(ilz,:,:)));
tmp = real(ifft2(squeeze(pvcheck(ilz,:,:))));
maxerr = max(max(abs(tmp - squeeze(pvxy(ilz,:,:)-mpv))));
disp(['maximum error in pv check at level ' int2str(ilz) ' = ' num2str(maxerr,4)])

gray = 0.3*[1 1 1];
if iplot
  %
  % PV and pressure 
  %
  fign = fign+1;
  figure(fign); clf
  % custom colormap to put white in the background field
  jet2 = jet; jet2 = [1 1 1; jet2];
  ilev = kmid+1; % pick a level for x-y plot
  h = pcolor(x*L/km,y*L/km,squeeze(EPVstar*pvxy(ilev,:,:))); 
  shading flat; 
  colormap(jet2); colorbar
  caxis([2e-7 8e-7]); cx = caxis; %colorbar
  hold on
  Pshear = Pref(ilev) - lambda*y*z(ilev);
  % 2D grid
  [A,Psxy] = meshgrid(ones(Nx,1),Pshear);
  dp = Rhoref(ilev)*g*60/hPa; % pressure contours equal to 60 m height lines
  pcints = [500:dp:600];
  [c,hl] = contour(x*L/km,y*L/km,Pstar*(squeeze(phixy(ilev,:,:))+Psxy)/hPa,pcints);
  set(hl,'linecolor','k','linewidth',1.5)
  fign = fign+1;
  xlabel('x(km)'); ylabel('y(km)'); title('PV and pressure')
  set(gca,'linewidth',2,'fontweight','bold')
  %
  % vorticity
  %
  figure(fign); clf
  [c,h] = contour(x*L/km,y*L/km,Pstar*(squeeze(phixy(ilev,:,:))+Psxy)/hPa);
  set(h,'linewidth',2)
  cx = caxis;
  % add vorticity contours
  vort = real(ifft2(-(DX.*DX + DY.*DY).*squeeze(phisp(ilev,:,:))));
  hold on
  vcints = [0:4:20]*1e-5;
  [c,hv] = contour(x*L/km,y*L/km,Qstar*vort,vcints);
  set(hv,'linecolor','k','linestyle','--')
  caxis(cx);
  xlabel('x(km)'); ylabel('y(km)'); title('pressure and vorticity')
  set(gca,'linewidth',2,'fontweight','bold')
end % iplot

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% "height" tendency calculation (really, pressure tendency)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if tendsolve

% compute nondimensional u, v, and PV advection on STAGGERED GRID (spectral)
% (really should compute these non-linear products using a de-alias technique)
% -([U + u'] * dQ'/dx) - (v' * dQ'/dy)
for k = 1:1:pmax
  pvsp = fft2(squeeze(pvxy(k,:,:)));
  dpvdx = real(ifft2(i*DX.*pvsp));
  dpvdy = real(ifft2(i*DY.*pvsp));
  uxy = real(ifft2(-i*DY.*squeeze(phisp(k,:,:))));
  vxy = real(ifft2( i*DX.*squeeze(phisp(k,:,:))));  
  pvadv(k,:,:) = -(Ub(k) + squeeze(uxy)).*dpvdx - squeeze(vxy).*dpvdy;
end

% lower boundary conditions from thermodynamic equation
% % -([U + u'] * dT'/dx) - (v' * dT'/dy)
thetabsp = fft2(thetabxy);
dthbdx = real(ifft2(i*DX.*thetabsp));
dthbdy = real(ifft2(i*DY.*thetabsp)) - lambda;
phibsp = fft2(phibxy);
ubxy = real(ifft2(-i*DY.*phibsp));
vbxy = real(ifft2( i*DX.*phibsp));
Ubb = lambda * zu(1) + Unot;
thbadv = -(Ubb + ubxy).*dthbdx - vbxy.*dthbdy;

% upper boundary conditions from thermodynamic equation
% % -([U + u'] * dT'/dx) - (v' * dT'/dy)
thetatsp = fft2(thetatxy);
dthtdx = real(ifft2(i*DX.*thetatsp));
dthtdy = real(ifft2(i*DY.*thetatsp)) - lambda;
phitsp = fft2(phitxy);
utxy = real(ifft2(-i*DY.*phitsp));
vtxy = real(ifft2( i*DX.*phitsp));
Ubt = lambda * zu(end) + Unot;
thtadv = -(Ubt + utxy).*dthtdx - vtxy.*dthtdy;

% invert pv/theta advection for pressure tendency
[ptxy,ptbxy,pttxy,junk,junk] = inv_laplacian(thbadv,thtadv,pvadv,idn,dz,facx,facy);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% repeat but for "temperature" and vorticity advection form of height tendency eqn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for k = 1:1:pmax
  % temperature advection
  if k == 1
	 tadv(1,:,:) = thbadv; % lower boundary advection from previous calc
  else
	 tusp = (squeeze(phisp(k,:,:) - phisp(k-1,:,:)))/dz; % spectral theta on ustaggered levels
	 dtdx = real(ifft2(i*DX.*tusp)); dtdy = real(ifft2(i*DY.*tusp)) - lambda; 
	 % need to average velocity components to unstaggered levels
	 uxy = real(ifft2(-i*DY.* (0.5*squeeze(phisp(k,:,:)+phisp(k-1,:,:))) ));
	 vxy = real(ifft2( i*DX.* (0.5*squeeze(phisp(k,:,:)+phisp(k-1,:,:))) ));
	 tadv(k,:,:) = -(Ubu(k) + squeeze(uxy)).*dtdx - squeeze(vxy).*dtdy;
    if k == pmax
		tadv(pmax+1,:,:) = thtadv; % upper boundary advection from previous calc
	 end
  end
  % vorticity advection (on staggered grid---need to recompute u an v!)
  vort = -(DX.*DX + DY.*DY).*squeeze(phisp(k,:,:));
  uxy = real(ifft2(-i*DY.*squeeze(phisp(k,:,:))));
  vxy = real(ifft2( i*DX.*squeeze(phisp(k,:,:))));  
  dvortdx = real(ifft2(i*DX.*vort));
  dvortdy = real(ifft2(i*DY.*vort));
  vadv(k,:,:) = -(Ub(k) + squeeze(uxy)).*dvortdx - squeeze(vxy).*dvortdy;
end
% compute vertical derivative of temperature advection; puts result back onto staggered grid
for k = 1:1:pmax
  tadvdz(k,:,:) = (tadv(k+1,:,:) - tadv(k,:,:))/dz;
end
% invert temperature advection for pressure tendency
[taptxy,taptbxy,tapttxy,junk,junk] = inv_laplacian(thbadv,thtadv,tadvdz,idn,dz,facx,facy);
% invert vorticity advection for pressure tendency
vadvbc = zeros(Ny,Nx); % zero boundary condition (all in temperature advection)
[vaptxy,vaptbxy,vapttxy,junk,junk] = inv_laplacian(vadvbc,vadvbc,vadv,idn,dz,facx,facy);
maxerr = max(max(max(abs(ptxy-vaptxy-taptxy))));
disp(['maximum error in temp-vort advection form of height tend eqn = ' num2str(maxerr,4)])

if iplot == 1

% pressure tendency along z midlevel
fign = fign+1;
figure(fign); clf
sfac = (Pstar/Tstar)*(86400/100); % Pa/s -> hPa/day
[c,h] = contourf(x*L/km,y*L/km,sfac*squeeze(ptxy(kmid,:,:))); colorbar
set(gca,'linewidth',2,'fontweight','bold');
xlabel('x(km)'); ylabel('y(km)'); title('pressure tendency (hPa/day)')

% pressure tendency along y midlevel
fign = fign+1;
figure(fign); clf
sfac = (Pstar/Tstar)*(86400/100); % Pa/s -> hPa/day
contourf(x*L/km,z*H/km,sfac*squeeze(ptxy(:,jmid,:))); colorbar
xlabel('x(km)'); ylabel('z(km)'); title('pressure tendency (hPa/day)')
set(gca,'linewidth',2,'fontweight','bold')

end % iplot

end % tendsolve

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% w calculation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if wsolve
  
% Q-vector
%   u,v derivative come straight from P field on staggered levels
%   theta derivatives are on unstaggered levels: average back to staggered

% compute theta on UNstaggered levels; u,v derivs on staggered levels
for k = 1:1:pmax
  pxx = real(ifft2( -DX.*DX.*squeeze(phisp(k,:,:))));
  pyy = real(ifft2( -DY.*DY.*squeeze(phisp(k,:,:))));
  pxy = real(ifft2( -DX.*DY.*squeeze(phisp(k,:,:))));
  if k < pmax 
	 dpz(k,:,:) = (phixy(k+1,:,:) - phixy(k,:,:))/dz;
  end
  % average d\phi/dz back to staggered levels where u,v derivatives are located
  if k == 1
	 dpzu = 0.5*(squeeze(dpz(k,:,:)) + thetabxy);
  elseif k == pmax
	 dpzu = 0.5*(squeeze(dpz(k-1,:,:)) + thetatxy);	 
  else
	 dpzu = 0.5*(squeeze(dpz(k,:,:)) + squeeze(dpz(k-1,:,:)));
  end
  % x,y derivatives of theta on unstaggered levels (including linear shear!)
  dtx = real(ifft2(i*DX.*fft2(dpzu)));
  dty = real(ifft2(i*DY.*fft2(dpzu))) - lambda;  
  % Q-vector on staggered levels
  Q1(k,:,:) = (pxy.*dtx) - (pxx.*dty);
  Q2(k,:,:) = (pyy.*dtx) - (pxy.*dty);  
  % Q-vector divergence
  Q1sp = fft2(squeeze(Q1(k,:,:))); Q2sp = fft2(squeeze(Q2(k,:,:)));
  Qdiv(k,:,:) = 2*real(ifft2(i*DX.*Q1sp + i*DY.*Q2sp));
  if k == kmid
	 dtxplt = dtx; pxxplt = pxx;
  end
end

% invert for w
lbcxy = zeros(Ny,Nx); ubcxy = zeros(Ny,Nx); 
idn = -1; % Dirichlet BC
[wxy,wbxy,wtxy,wzbxy,wztxy] = inv_laplacian(lbcxy,ubcxy,Qdiv,idn,dz,facx,facy);

if iplot == 1

% vertical motion along z midlevel
fign = fign+1;
figure(fign); clf
sfac = Wstar*100; % cm/s
[c,h] = contourf(x*L/km,y*L/km,sfac*squeeze(wxy(kmid,:,:))); colorbar;
xlabel('x(km)'); ylabel('y(km)'); title('vertical motion (cm/s)')
set(gca,'linewidth',2,'fontweight','bold')

% vertical motion along y midlevel
fign = fign+1;
figure(fign); clf
contourf(x*L/km,z*H/km,sfac*squeeze(wxy(:,jmid,:))); colorbar
xlabel('x(km)'); ylabel('z(km)'); title('vertical motion (cm/s)')
set(gca,'linewidth',2,'fontweight','bold')

fign = fign+1;
figure(fign); clf
quiver(x*L/km,y*L/km,squeeze(Q1(kmid,:,:)),squeeze(Q2(kmid,:,:)));
xlabel('x (km)'); ylabel('y (km)'); title('Q vector')
set(gca,'linewidth',2,'fontweight','bold')
end % iplot

end % wsolve

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% divergent ageostrophic circulation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ageosolve
  
disp(['solving for (u_a, v_a) on staggered grid...'])
for p = 1:1:pmax
  if p == 1
	 dwdzxy = squeeze(wxy(p,:,:))*2/dz; 
  elseif p == pmax
	 dwdzxy = -squeeze(wxy(p,:,:))*2/dz; 
  else
	 dwdzxy = squeeze((wxy(p,:,:) - wxy(p-1,:,:)))/dz;
  end
  dwdzsp = fft2(dwdzxy);
  chisp = zeros(Ny,Nx);
  for k=1:1:2*kmax; 
	 for l=1:1:2*lmax;
		[ak,bl] = get_waves(k,l,kmax,lmax,facx,facy);
		if ((k == 1) & (l == 1)) % no information on (x,y) mean
		  chisp(l,k) = 0;
		else
		  chisp(l,k) = dwdzsp(l,k)/(ak.^2 + bl.^2);
		end
	 end % l
  end % k
  chixy(p,:,:) = real(ifft2(chisp));
  uasp = i*DX.*chisp; vasp = i*DY.*chisp;
  uaxy(p,:,:) = real(ifft2(uasp)); vaxy(p,:,:) = real(ifft2(vasp));
  % mass continuity check
  %ux = real(ifft2(i*DX.*uasp)); vy = real(ifft2(i*DY.*vasp));
  %cc = max(max(abs(ux+vy+dwdzxy))); 
  %disp([int2str(p) ' maximum error in mass continuity check...' num2str(cc,4)]);
end

if iplot == 1

fign = fign+1;
figure(fign); clf
quiver(x*L/km,y*L/km,squeeze(uaxy(kmid,:,:)),squeeze(vaxy(kmid,:,:)));
set(gca,'linewidth',2,'fontweight','bold')
xlabel('x (km)'); ylabel('z (km)'); title('(u_a, v_a)')
set(gca,'linewidth',2,'fontweight','bold')

fign = fign+1;
figure(fign); clf
quiver(squeeze(uaxy(:,jmid,:)),squeeze(wxy(:,jmid,:)));
axis tight
set(gca,'linewidth',2,'fontweight','bold')
xlabel('x'); ylabel('z'); title('(u_a, w)')
set(gca,'linewidth',2,'fontweight','bold')

end % iplot

end % ageosolve

