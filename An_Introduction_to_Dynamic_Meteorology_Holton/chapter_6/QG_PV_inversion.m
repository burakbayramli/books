%
% invert a QG PV distribution subject to specified boundary conditions
%
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-10 12:09:59 -0800 (Fri, 10 Feb 2012) $
% $Revision: 790 $
% $Author: hakim $
% $Id: QG_PV_inversion.m 790 2012-02-10 20:09:59Z hakim $

%
% Notes:
%

clear

% menu options (1 turns on; 0 turns off)
iplot     = 1; % plots
echeck    = 0; % error check

% initialize computational grid, scaling parameters, and vertical reference state
grid_setup;
scaling;
reference_state;

zlev = kmid+1; % pick a z level for x-y plot
ylev = jmid+1; % pick a y level for x-z plot

% initialize figure number
fign = 0;

%
% specify a PV distribution (pvmag in PVU; thmag in K)
% built-in options:
% ipv == 1 % pv parallelpiped 
% ipv == 3 % pv gaussian (cylinder in upper part of domain)
% ipv == 4 % 3D pv gaussian 
% ipv == 5 % 3D pv gaussian elliptical dipole ("jet streak")
% ipv == 6 % zero pv and a gaussian surface warm anomaly
% ipv == 61 % zero pv and a gaussian tropopause cold anomaly
% ipv == 62 % pv gaussian as in option 3 and a gaussian tropopause anomaly

ipv = 1; pvmag = 2e-6; thmag = 0; % interior PV blob with no boundary theta
%ipv = 5; pvmag = -4e-6; thmag = 0; % interior PV dipole ("jetstreak") 

% now initialize the PV and boundary temperature
QG_initial_value;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% invert PV/theta for pressure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% zero theta BCs (requires zero volume integrated PV)
idn = 1; % Nuemann BC (zero theta)
%idn = -1; % Dirichlet BC (zero pressure)
pvxy = pvxy - mean(mean(mean(pvxy)))*XL*YL*ZH;

% call generic solver (phixy is on staggered grid)
[phixy,phibxy,phitxy,thetabxy,thetatxy] = inv_laplacian(lbcxy,ubcxy,pvxy,idn,dz,facx,facy);

% lowest and highest values globally and on loweer surface
disp(['domain minimum pressure anomaly: ' num2str(min(min(min(Pstar*phixy/hPa))),3) ' hPa'])
disp(['domain maximum pressure anomaly: ' num2str(max(max(max(Pstar*phixy/hPa))),3) ' hPa'])
disp(['surface minimum pressure anomaly: ' num2str(min(min(Pstar*phibxy/hPa)),3) ' hPa'])
disp(['surface maximum pressure anomaly: ' num2str(max(max(Pstar*phibxy/hPa)),3) ' hPa'])

% spectral pressure
for k = 1:1:pmax
  phisp(k,:,:) = fft2(squeeze(phixy(k,:,:)));
end
tbsp = fft2(thetabxy); ttsp = fft2(thetatxy);

if echeck
  % check the PV by direct calculation (using spectral fields)
  % need these fields for later calculations anyway!
  disp(['...checking PV...'])
  pvcheck = laplacian(nlevs,phisp,tbsp,ttsp,DX,DY,dz);
  
  % process and check at one (y,z)
  ilz = kmid; 
  mpv = mean(mean(pvxy(ilz,:,:)));
  tmp = real(ifft2(squeeze(pvcheck(ilz,:,:))));
  maxerr = max(max(abs(tmp - squeeze(pvxy(ilz,:,:)-mpv))));
  disp(['maximum error in pv check at level ' int2str(ilz) ' = ' num2str(maxerr,4)])
end

% plot a gray patch around a single PV contour
PVcontour = 5e-7; % PVU
gray = 0.5*[1 1 1];
if iplot
  %
  % (x,y) PV (color or grayshade) and pressure 
  %
  fign = fign+1;
  figure(fign); clf
  % account for dipole
  if ipv == 5 | ipv == 7
	 [c,h] = contourf(x*L/km,y*L/km,squeeze(EPVstar*pvxy(zlev,:,:)),2); set(h,'linecolor','w')
	 setcolor; colormap(hj16); cx = caxis;
  else
	 % just plot single PV contour
	 [c,h] = contourf(x*L/km,y*L/km,squeeze(EPVstar*pvxy(zlev,:,:)),[1 1]*PVcontour);colormap([1 0 0]); set(h,'linecolor','w'); cx = caxis;
  end
  hold on
  % now add pressure contours to the plot
  % option to plot in hPa or in equivalent hydrostatic height
  dp = 4; % hPa
  %dh = 30; dp = Rhoref(zlev)*g*60/hPa; % pressure contours equal to dh height lines (in m)
  % contours:
  pcints = [-10*dp:dp:10*dp];
  [c,hl] = contour(x*L/km,y*L/km,Pstar*(squeeze(phixy(zlev,:,:)))/hPa,nonzeros((pcints<0).*pcints)); set(hl,'linecolor','k','linewidth',1.5,'linestyle','--')
  [c,hl] = contour(x*L/km,y*L/km,Pstar*(squeeze(phixy(zlev,:,:)))/hPa,[0; nonzeros((pcints>0).*pcints)]); set(hl,'linecolor','k','linewidth',1.5,'linestyle','-')
  xlabel('x (km)'); ylabel('y (km)');
  title(['PV (color) and pressure (contours every ' int2str(dp) ' hPa)'])
  caxis(cx);

  %
  % (x,z) PV (color or grayshade) and pressure 
  %
  fign = fign+1;
  figure(fign); clf
  % account for dipole
  if ipv == 5 | ipv == 7
	 [c,h] = contourf(x*L/km,z*H/km,squeeze(EPVstar*pvxy(:,ylev,:)),2); set(h,'linecolor','w')
	 setcolor; colormap(hj16); cx = caxis;
  else
	 [c,h] = contourf(x*L/km,z*H/km,squeeze(EPVstar*pvxy(:,ylev,:)),[1 1]*PVcontour);colormap([1 0 0]); set(h,'linecolor','w'); cx = caxis;
  end
  hold on
  % now add pressure contours to the plot
  % option to plot in hPa or in equivalent hydrostatic height
  dp = 4; % hPa
  %dh = 30; dp = Rhoref(zlev)*g*60/hPa; % pressure contours equal to dh height lines (in m)
  % contours:
  pcints = [-10*dp:dp:10*dp];
  [c,hl] = contour(x*L/km,z*H/km,Pstar*(squeeze(phixy(:,ylev,:)))/hPa,nonzeros((pcints<0).*pcints)); set(hl,'linecolor','k','linewidth',1.5,'linestyle','--')
  [c,hl] = contour(x*L/km,z*H/km,Pstar*(squeeze(phixy(:,ylev,:)))/hPa,[0; nonzeros((pcints>0).*pcints)]); set(hl,'linecolor','k','linewidth',1.5,'linestyle','-')
  xlabel('x (km)'); ylabel('z (km)');
  title(['PV (color) and pressure (contours every ' int2str(dp) ' hPa)'])
  caxis(cx);

  %
  % (x,y) PV (color or grayshade) and vorticity
  %
  fign = fign+1;
  figure(fign); clf
  % account for dipole
  if ipv == 5 | ipv == 7
	 [c,h] = contourf(x*L/km,y*L/km,squeeze(EPVstar*pvxy(zlev,:,:)),2); set(h,'linecolor','w')
	 setcolor; colormap(hj16); cx = caxis;
  else
	 % just plot single PV contour
	 [c,h] = contourf(x*L/km,y*L/km,squeeze(EPVstar*pvxy(zlev,:,:)),[1 1]*PVcontour);colormap([1 0 0]); set(h,'linecolor','w'); cx = caxis;
  end
  hold on;
  % compute vorticity
  vort = real(ifft2(-(DX.*DX + DY.*DY).*squeeze(phisp(zlev,:,:))));
  % add vorticity contours
  dv = 4; % x 10^-5 s^-1
  vcints = [-10*dv:dv:10*dv]*1e-5; % s^-1
  [c,hv] = contour(x*L/km,y*L/km,Qstar*vort,nonzeros((vcints<0).*vcints)); set(hv,'linewidth',1.5,'linecolor','k','linestyle','--');
  [c,hv] = contour(x*L/km,y*L/km,Qstar*vort,[0; nonzeros((vcints>0).*vcints)]); set(hv,'linewidth',1.5,'linecolor','k','linestyle','-');
  caxis(cx);
  title(['PV (color) and vorticity (contours every ' int2str(dv) ' x10^{-5} s^{-1})'])
end % iplot

% here's how to compute the geostrophic wind (u,v)
uxy = real(ifft2(-i*DY.*squeeze(phisp(k,:,:))));
vxy = real(ifft2( i*DX.*squeeze(phisp(k,:,:))));  
