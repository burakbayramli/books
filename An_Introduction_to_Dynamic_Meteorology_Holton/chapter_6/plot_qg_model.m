%
% make plots after running the qg model
%
% runs on data in memory or from a file
% 
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-10 12:09:59 -0800 (Fri, 10 Feb 2012) $
% $Revision: 790 $
% $Author: hakim $
% $Id: plot_qg_model.m 790 2012-02-10 20:09:59Z hakim $

% set data source
idata     = 0; % 0 = data stored in memory (just ran model); 1 = read file 

% option to start with clear memory and read from a file; set 1 == 0 to run from memory
if idata
  clear
  load QGout_20120208.214941.mat; % enter a filename produced by QG_model.m
  lambda = 1;
  scaling; grid_setup; 
end
reference_state

% select a time slice here
it = 2;

disp(['displaying time slice ' int2str(it) ' of ' int2str(size(Xf,1))]);

% set pressure contours (perturbation only)
dp = 2; pcints = [-10*dp:dp:10*dp]; % hPa

% set theta contours (total)
tcints = [280:4:340]; % K

% tick marks
ticks = [-4000:1000:4000];

if it < 0 | it > length(tspan)
  error(['time out of bounds. it=' int2str(it) ' number of times=' int2str(length(tspan))])
else
  disp(['time selected is ' num2str(tspan(it)*Tstar/3600) ' hours'])
  %disp(['time selected is ' num2str(tspan(it)*Tstar/86400) ' days'])  
end

xin = Xf(it,:);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extract pv, theta_b, and theta_t from the column vector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pv = reshape(xin(1:Nx*Ny*Nz),Nz,Ny,Nx);
lb = (Nx*Ny*Nz)+1;
theta_b = reshape(xin(lb:lb+(Nx*Ny)-1),Ny,Nx);
lb = lb+(Nx*Ny);
theta_t = reshape(xin(lb:end),Ny,Nx);

% basic-state theta field
if ijet 
  tmp = -lambda*y; [junk,THbar_shear] = meshgrid(ones(Nx,1),tmp);
  THbar_b = THbar_shear;
  THbar_t = THbar_shear;
  % basic state pressure on tropopause
  Pshear = Pref(end) - lambda*y;
  % 2D grid
  [A,Ptxy] = meshgrid(ones(Nx,1),Pshear);
else
  THbar_b = zeros(size(theta_b));
  THbar_t = zeros(size(theta_t));
  Psxy = zeros(size(theta_t));
end

% invert for the pressure field
idn = 1; % Nuemann BC 
[phixy,phibxy,phitxy,thetabxy,thetatxy] = inv_laplacian(theta_b,theta_t,pv,idn,dz,facx,facy);

%disp(['minimum surface pressure perturbation:  ' num2str(min(min(phibxy))*Pstar/hPa) ' hPa'])

% dimensional coordinate variables
xd = x*L/km; yd = y*L/km; zd = z*H/km;

fign = 0;

% PV cross section along z midlevel
fign = fign + 1; figure(fign); clf
contourf(xd,yd,squeeze(pv(kmid,:,:))*EPVstar*pvu); colorbar
xlabel('x (km)'); ylabel('y (km)'); title('PV (PVU)');

% PV cross section along y midlevl
fign = fign + 1; figure(fign); clf
contourf(xd,zd,squeeze(pv(:,jmid,:))*EPVstar*pvu); colorbar
xlabel('x (km)'); ylabel('z (km)'); title('PV (PVU)');

% surface theta and pressure
fign = fign + 1; figure(fign); clf
% color theta plot if there's something to show
tcheck = max(max(theta_b)) ~= min(min(theta_b));
if tcheck
  pcolor(xd,yd,(theta_b+THbar_b)*Thstar+Thnot); shading interp; colorbar; 
  cx = caxis;
  hold on;
  ttl = 'surface pressure (hPa) and potential temperature (K)';
else
  ttl = 'surface pressure (hPa)';
end
% pressure:
contour(xd,yd,phibxy*Pstar/hPa,pcints,'k-'); title(ttl);
if tcheck; caxis(cx); end
xlabel('x (km)'); ylabel('z (km)'); 

% tropopause theta and pressure
fign = fign + 1; figure(fign); clf
% color theta plot if there's something to show
tcheck = max(max(theta_t)) ~= min(min(theta_t));
if tcheck
  pcolor(xd,yd,(theta_t+THbar_t)*Thstar+Thnot); shading interp; colorbar; 
  cx = caxis;
  hold on;
  ttl = 'tropopause pressure (hPa) and potential temperature (K)';
else
  ttl = 'tropopause pressure (hPa)';
end
% pressure:
tpcints = pcints + Pref(end)*Pstar/hPa;
contour(xd,yd,(phitxy+Ptxy)*Pstar/hPa,tpcints,'k-'); title(ttl);
if tcheck; caxis(cx); end
xlabel('x (km)'); ylabel('z (km)'); 

