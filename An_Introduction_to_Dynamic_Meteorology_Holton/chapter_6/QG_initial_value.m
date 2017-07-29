%
% initial condition specification
% set pvmag, thmag (magnitude of pv & theta anomalies) and ipv (anomaly selector) beforehand
%
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-10 12:09:59 -0800 (Fri, 10 Feb 2012) $
% $Revision: 790 $
% $Author: hakim $
% $Id: QG_initial_value.m 790 2012-02-10 20:09:59Z hakim $

disp([' ']); disp(['initial condition specification...'])

tic

% check to make sure the user has set up everything beforehand
if exist('EPVstar') == 0; error('run scaling.m before calling QG_initial_value'); end
if exist('xg') == 0; error('run grid_setup.m before calling QG_initial_value'); end
if exist('ipv') == 0; error('set ipv initial condition flag before calling QG_initial_value'); end
if exist('pvmag') & exist ('thmag')
  % nondimensionalized magnitude of input PV perturbation (input: Ertel PV units)
  pvnd = pvmag/EPVstar; 
  % nondimensionalized magnitude of input surface theta perturbation (input: K)
  thnd = thmag/Thstar;
else
  error('set pvmag and thmag before calling QG_initial_value')
end

% initialize fields to zero
lbcxy = zeros(Ny,Nx); ubcxy = zeros(Ny,Nx); pvxy = zeros(pmax,Ny,Nx);

% specify PV distribution (blob with zero BC) STAGGERED GRID
lbcxy = zeros(Ny,Nx); ubcxy = zeros(Ny,Nx); pvxy = zeros(pmax,Ny,Nx);

if ipv == 1 % pv parallelpiped (corner artifacts except at small amplitude)
  iw = 2; pvxy(kmid-iw:kmid+iw,kmax-iw:kmax+iw,lmax-iw:lmax+iw) = pvnd;
elseif ipv == 2 %  pv cylinder
  asx = 1; asy = asx;
  rr = sqrt(((xg/asx).^2) + ((yg/asy).^2));
  rmask = rr < 1; % identify a circle
  iw = 8; % half-depth of PV layer in vertical grid units
  for k = kmid-iw:kmid+iw
	 pvxy(k,:,:) = pvnd*rmask;
  end
elseif ipv == 3 % pv gaussian (cylinder in upper part of domain)
  asx = .35; asy = asx; % this controls the shape and spatial extent
  rr = sqrt(((xg/asx).^2) + ((yg/asy).^2));
  %iw = 8; % half-depth of PV layer in vertical grid units
  %for k = kmid-iw:kmid+iw
  % use this for cyclone development
  for k = fix(Nz*.75):1:Nz
	 pvxy(k,:,:) = pvnd*exp(-rr.^2);;
  end
elseif ipv == 4 % 3D pv gaussian
  % use this for Chapter 6 figures
  asx = .25; asy = asx; asz = .1; % this controls the shape and spatial extent
  xnot = x(fix(imid)); ynot = y(jmid); znot = z(kmid); % location
  rr = sqrt((((xg-xnot)/asx).^2) + (((yg-ynot)/asy).^2));
  for k = 1:1:Nz
	 pvxy(k,:,:) = pvnd*exp(-rr.^2)*exp(-((z(k)-znot)/asz).^2);
  end
elseif ipv == 41 % two 3D pv gaussian vortices
  asx = .25; asy = asx; asz = .1; % this controls the shape and spatial extent
  xnot = x(fix(imid-2)); ynot = y(jmid); znot = z(kmid); % location
  rr = sqrt((((xg-xnot)/asx).^2) + (((yg-ynot)/asy).^2));
  for k = 1:1:Nz
	 pvxy(k,:,:) = pvnd*exp(-rr.^2)*exp(-((z(k)-znot)/asz).^2);
  end
  xnot = x(fix(imid+2)); ynot = y(jmid); znot = z(kmid); % location
  rr = sqrt((((xg-xnot)/asx).^2) + (((yg-ynot)/asy).^2));
  for k = 1:1:Nz
	 pvxy(k,:,:) = squeeze(pvxy(k,:,:)) + pvnd*exp(-rr.^2)*exp(-((z(k)-znot)/asz).^2);
  end
elseif ipv == 5 % 3D pv gaussian elliptical dipole ("jet streak")
  asx = .7; asy = asx/2; asz = .1; % this controls the shape and spatial extent
  ynot1 = 0.5; ynot2 = -0.5;
  rr1 = sqrt(((xg/asx).^2) + (((yg-ynot1)/asy).^2));
  rr2 = sqrt(((xg/asx).^2) + (((yg-ynot2)/asy).^2));
  for k = 1:1:Nz
	 pvxy(k,:,:) = pvnd*exp(-rr1.^2)*exp(-((z(k)-z(kmid))/asz).^2);
	 pvxy(k,:,:) = squeeze(pvxy(k,:,:)) - pvnd*exp(-rr2.^2)*exp(-((z(k)-z(kmid))/asz).^2);
  end
elseif ipv == 6 % zero pv and a gaussian surface warm anomaly
  asx = .35; asy = asx; % this controls the shape and spatial extent
  rr = sqrt(((xg/asx).^2) + ((yg/asy).^2));
  lbcxy = thnd*exp(-rr.^2);;
elseif ipv == 61 % zero pv and a gaussian tropopause cold anomaly
  asx = .35; asy = asx; % this controls the shape and spatial extent
  rr = sqrt(((xg/asx).^2) + ((yg/asy).^2));
  ubcxy = thnd*exp(-rr.^2);;
elseif ipv == 62 % pv gaussian as in option 3 and a gaussian tropopause anomaly
  asx = .35; asy = asx; % this controls the shape and spatial extent
  xnot = x(fix(imid)); ynot = y(jmid); 
  rr = sqrt((((xg-xnot)/asx).^2) + (((yg-ynot)/asy).^2));
  ubcxy = thnd*exp(-rr.^2);;
  for k = fix(Nz*.75):1:Nz
	 pvxy(k,:,:) = pvnd*exp(-rr.^2);;
  end
elseif ipv == 7 % pv plane wave in x, independent of y and z
  for k = 1:1:Nz
	 pvxy(k,:,:) = pvnd*sin(xg*pi/xg(end,end));
  end
else
  error('wrong specification of pv anomaly selector ipv')
end
toc

% zero volume-integrated PV should be zero
pvxy = pvxy - mean(mean(mean(pvxy))); 

%
% END: IC specification
%
