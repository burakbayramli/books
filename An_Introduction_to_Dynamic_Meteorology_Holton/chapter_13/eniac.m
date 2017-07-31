
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%                                 %%%%%%%%%%%%%%%
%%%%%%%%%%%             ENIAC               %%%%%%%%%%%%%%%
%%%%%%%%%%%                                 %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% From https://maths.ucd.ie/~plynch/eniac/
%%%                                                     %%%
%%%     Solve the Barotropic Vorticity Equation         %%%
%%%                                                     %%%
%%%        d/dt (Del^2(z)) = J(h*Del^2(z)+f,z)  (1)     %%%
%%%                                                     %%%
%%%   where h = g*m^2/f with map factor m.              %%%
%%%                                                     %%%
%%%   The formulation is the same as used by            %%%
%%%   Charney, Fjortoft and von Neumann (1950) in       %%%
%%%   the ENIAC integrations (Tellus, 2, 237-254).      %%%
%%%                                                     %%%
%%%   An alternative formulation of the BVE using       %%%
%%%   the stream function is  used if the indicator     %%%
%%%               StreamFunction = 1                    %%%
%%%   It is formally identical to (1) above, but now    %%%
%%%   z stands for the streamfunction and h has a       %%%
%%%   different definition: h = m^2.                    %%%
%%%                                                     %%%
%%%   The initial data are as used in tho original      %%%
%%%   integrations in 1950. The boundary conditions     %%%
%%%   are formulated in the same way.                   %%%
%%%                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                     %%%
%%%   The Barotropic Vorticity Equation is approximated %%%
%%%   by centered finite differences. The domain has    %%%
%%%   M * N points.                                     %%%
%%%                                                     %%%
%%%   The boundary conditions are as described          %%%
%%%   in the Tellus paper: z is given on the boundary   %%%
%%%   and vorticity (xi) is given at inflow points.     %%%
%%%                                                     %%%
%%%   The time scheme is the leapfrog method.           %%%
%%%   (the first step is a forward step).               %%%
%%%                                                     %%%
%%%   The quantity to be stepped forward is             %%%
%%%                xi = Del^2(z)                        %%%
%%%   Thus, after each time-step, it is necessary       %%%
%%%   to solve a Poisson equation to get z.             %%%
%%%   This is done by the Fourier Transform method,     %%%
%%%   as in the Tellus paper. (For a description of     %%%
%%%   the method, see Numerical Recipes Sec. 19.4.)     %%%
%%%                                                     %%%
%%%   A variety of initial conditions may be specified  %%%
%%%   determined by the parameter Ncase (see below).    %%%
%%%                                                     %%%
%%%   The data were downloaded from the NCEP/NCAR       %%%
%%%   Reanalysis site. See, for example,                %%%
%%%          http://nomad3.ncep.noaa.gov/               %%%
%%%                 cgi-bin/ftp2u_6p_r1.sh              %%%
%%%                                                     %%%
%%%   They were converted from GRIB to ASCII and        %%%
%%%   saved in files with the name YYYYMMDDHH.asc       %%%
%%%   where YYYYMMDDHH is the date and time of the      %%%
%%%   analysis. Thus, for example, for Ncase=1,         %%%
%%%   the file is 1949010503.asc.                       %%%
%%%                                                     %%%
%%%   The re-analysis is on a 2.5 x 2.5 degree grid.    %%%
%%%   The data were recovered on a domain covering      %%%
%%%   the northern hemisphere. The fields were          %%%
%%%   interpolated to the 19 x 16 polar stereographic   %%%
%%%   grid used by Charney et al, and the results       %%%
%%%   saved in files with the names YYYYMMDDHH.z00      %%%
%%%                                                     %%%
%%%   The four cases in question are (all in 1949):     %%%
%%%                                                     %%%
%%%     (1) 0300 Z, January 5th, 1949                   %%%
%%%         with verification analysis Jan 6            %%%
%%%     (2) 0300 Z, January 30th, 1949                  %%%
%%%         with verification analysis Jan 31           %%%
%%%     (3) 0300 Z, January 31st, 1949                  %%%
%%%         with verification analysis Feb 1            %%%
%%%     (4) 0300 Z, February 13th, 1949                 %%%
%%%         with verification analysis Feb 14           %%%
%%%                                                     %%%
%%%   Note: Charney et al. used the analyses valid      %%%
%%%   at 0300 Z. These were not immediately available   %%%
%%%   from the re-analysis site.                        %%%
%%%   Thanks to Chi-Fan Shih for assistance in          %%%
%%%   acquiring these 0300Z fields.                     %%%
%%%                                                     %%%
%%% Ncase    Analysis file        Verification file     %%%
%%%  1    Case1-1949010503.z00   Case1-1949010603.z00   %%%
%%%  2    Case2-1949013003.z00   Case2-1949013103.z00   %%%
%%%  3    Case2-1949013103.z00   Case2-1949020103.z00   %%%
%%%  4    Case2-1949002133.z00   Case2-1949021403.z00   %%%
%%%                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                     %%%
%%% Date: February, 2007                                %%%
%%%                                                     %%%
%%% Author: Peter Lynch, UCD Met & Climate Centre       %%%
%%%                      University College Dublin      %%%
%%%                                                     %%%
%%% Email:  Peter.Lynch@ucd.ie                          %%%
%%%                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  diary DIARY %  SAVE OUTPUT IN DIARY IF REQUIRED.

clear    %    Clear the memory.
clf      %    clear the display.

%%% Define the graphic display colours.
whitebg([1 1 0.50]);  %  Beige background.
colormap('jet');    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Basic parameters that may be varied

    Ncase   = 1;   %  Forecast case number.
    DAYLEN  = 1;   %  Forecast length in days.
    DtHours = 1;   %  Timestep in hours.

%%  Indicator for psi-form of equation
    StreamFunction = 0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Details of polar stereographic grid
%    M:   (= 19) points in x-direction
%    N:   (= 16) points in y-direction
%    Ds:  (= 736 km) Grid step at North Pole
%    Xp:   x-coordinate of North Pole (varies)
%    Yp:   y-coordinate of North Pole (varies)
%    Centerangle: Angle between Date-line and
%                 positive y axis (varies).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Set the PS grid parameters for 4 cases.

NUMx = [ 19 19 19 19 ];   % x dimension
NUMy = [ 16 16 16 16 ];   % y dimension
Xpol = [ 10 10 10 10 ];   % North Pole
Ypol = [ 14 12 14 14 ];   % North Pole
DELs = [ 736E+3 736E+3 736E+3 736E+3];
Cang = [ -90 -70 -35 -85 ];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Set the parameters for the forecast (Ncase)

M  = NUMx(Ncase); % Points in x direction
N  = NUMy(Ncase); % Points in y direction
Xp = Xpol(Ncase); % Coords of North Pole
Yp = Ypol(Ncase); % Coords of North Pole
Ds = DELs(Ncase); % Grid step at NP (metres)
Centerangle = Cang(Ncase); % Central angle of map.

MN   = M*N;       %  Total number of grid points.

%% Define the spatial grid (not used in this form).
%% x = (0:M-1) * Ds;   % x-coordinates of grid points
%% y = (0:N-1) * Ds;   % y-coordinates of grid points

% Define the (X,Y) grid (for plotting)
[X, Y ] = meshgrid(1:M,1:N);
X = X'; Y=Y';    % Transpose for standard plot.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Define the time variable

daylen = DAYLEN;           %  Integration time (in days)
seclen = daylen*24*60*60;  %  Integration time (in seconds)
Dt = DtHours*60*60;        %  Timestep in seconds
nt = seclen/Dt;            %  Total number of time-steps.

t = (0:nt)*Dt;             %  Time variable
time = t/(60*60);          %  Time in hours (for plotting)
nspd = (24*60*60)/Dt;      %  Time steps per day

%%  Print out information on space and time grid.
fprintf('Ncase = %i \n', Ncase)
fprintf('Grid size, M=%i  N=%i \n',M,N)
fprintf('Total grid points, M*N=%i \n',MN)
fprintf('Timesteps per day, nspd=%i \n',nspd)
fprintf('Stream function, %i \n',StreamFunction)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Define some geophysical constants (SI units)

a = (4*10^7)/(2*pi);      %  Radius of the Earth
grav = 9.80665;           %  Gravitational acceleration
Omega = 2*pi/(24*60*60);  %  Angular velocity of Earth.
f0 = 2*Omega*sin(pi/4);   %  Mean Coriolis parameter.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Compute latitude and longitude
%%  on the polar stereographic grid.

r2d = 180/pi;  % Conversion factor: radians to degrees.
d2r = pi/180;  % Conversion factor: degrees to radians.

for ny=1:N
for nx=1:M
  xx = (nx-Xp)*Ds;
  yy = (ny-Yp)*Ds;
  rr = sqrt(xx^2+yy^2);
  theta = atan2(yy,xx);
  lambda = theta + d2r*(90+Centerangle);
  if(lambda>pi) lambda = lambda - 2*pi; end
  phi = 2*((pi/4)-atan(rr/(2*a)));
  LON(nx,ny) = lambda;   %  Longitude (radians)
  LAT(nx,ny) = phi;      %  Latitude  (radians)

  %%  Angles in degrees (for plotting)
  lamd = r2d*lambda; phid = r2d*phi;
  LONDEG(nx,ny) = lamd;  %  Longitude (degrees)
  LATDEG(nx,ny) = phid;  %  Latitude  (degrees)

end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Record the latitude and longitude of the
%%  corners of the outer domain and of the
%%  inner domain (for checking purposes).
Corners = 0;
if(Corners == 1 )
  latSW = LATDEG(1,1); lonSW = LONDEG(1,1);
  latSE = LATDEG(M,1); lonSE = LONDEG(M,1);
  latNE = LATDEG(M,N); lonNE = LONDEG(M,N);
  latNW = LATDEG(1,N); lonNW = LONDEG(1,N);
  fprintf(' Corner latitudes and longitudes, OUTER DOMAIN \n');
  fprintf(' Lat and long of SW corner %g %g \n',latSW,lonSW);
  fprintf(' Lat and long of SE corner %g %g \n',latSE,lonSE);
  fprintf(' Lat and long of NE corner %g %g \n',latNE,lonNE);
  fprintf(' Lat and long of NW corner %g %g \n',latNW,lonNW);
          
  latSW = LATDEG(3  ,2  ); lonSW = LONDEG(3  ,2  );
  latSE = LATDEG(M-2,2  ); lonSE = LONDEG(M-2,2  );
  latNE = LATDEG(M-2,N-2); lonNE = LONDEG(M-2,N-2);
  latNW = LATDEG(3  ,N-2); lonNW = LONDEG(3  ,N-2);
  fprintf(' Corner latitudes and longitudes, INNER DOMAIN \n');
  fprintf(' Lat and long of SW inner  %g %g \n',latSW,lonSW);
  fprintf(' Lat and long of SE inner  %g %g \n',latSE,lonSE);
  fprintf(' Lat and long of NE inner  %g %g \n',latNE,lonNE);
  fprintf(' Lat and long of NW inner  %g %g \n',latNW,lonNW);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Compute Coriolis Parameter and Map Factor
%%  and parameter h = g*m^2/f used in the BVE
%%  (For psi-equation, h = m^2 is used).

for ny=1:N
for nx=1:M
  lambda = LON(nx,ny);
  phi = LAT(nx,ny);
  map = 2 / (1+sin(phi));
  f = 2*Omega*sin(phi);
  MAP(nx,ny) = map;
  FCOR(nx,ny) = f;
  h(nx,ny) = grav * map^2 / f;
  if (StreamFunction == 1) h(nx,ny) = map^2; end
end
end

clf;  %  Clear the plot window
PlotAll = 0;
if(PlotAll==1)
  subplot(2,2,1)
  contourf(X,Y,MAP);  title('MAP FACTOR'); 
  subplot(2,2,2)
  contourf(X,Y,FCOR); title('f CORIOLIS');
  subplot(2,2,3)
  contourf(X,Y,h);    title('FACTOR h');
  pause
  clf
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Compute Sine Matrices for the Poisson solver

%%  Coefficients for x-transformation
for m1=1:M-2
for m2=1:M-2
  SM(m1,m2) = sin(pi*m1*m2/(M-1));
end
end

%%  Coefficients for y-transformation
for n1=1:N-2
for n2=1:N-2
  SN(n1,n2) = sin(pi*n1*n2/(N-1));
end
end

%%  Eigenvalues of Laplacian operator
for mm=1:M-2
for nn=1:N-2
  eigen = (sin(pi*mm/(2*(M-1))))^2 +(sin(pi*nn/(2*(N-1))))^2;
  EIGEN(mm,nn) = (-4/Ds^2) * eigen;
end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Read and plot the initial and
%%%   verification height data

%%%   Define the initial field sizes.
z0    = zeros(M,N);  %  Initial height
z24   = zeros(M,N);  %  Verifying Analysis
xi0   = zeros(M,N);  %  Initial Laplacian of height
eta0  = zeros(M,N);  %  Initial absolute vorticity

%% File name tag.
Case(1,1:6) = 'Case1-';
Case(2,1:6) = 'Case2-';
Case(3,1:6) = 'Case3-';
Case(4,1:6) = 'Case4-';

%% Initial analysis
YMDH1(1,1:10) = '1949010503';
YMDH1(2,1:10) = '1949013003';
YMDH1(3,1:10) = '1949013103';
YMDH1(4,1:10) = '1949021303';

%% Verifying analysis
YMDH2(1,1:10) = '1949010603';
YMDH2(2,1:10) = '1949013103';
YMDH2(3,1:10) = '1949020103';
YMDH2(4,1:10) = '1949021403';

%% Initial and verification analysis on PS grid
File1 = [Case(Ncase,:) YMDH1(Ncase,:) '.z00'];
File2 = [Case(Ncase,:) YMDH2(Ncase,:) '.z00'];

%%  Read in the analyses on the PS grid
z0  = load(File1);  %  Initial height analysis
z24 = load(File2);  %  Verifying height analysis

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Plot the analysis fields

zcontours = 4500:50:6000;
contourf(X,Y,z0,zcontours); title('Initial Data')
pause
contourf(X,Y,z24,zcontours); title('Final Analysis')
pause

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Plot inner region: omit one row at bottom
%%  and two rows on the other three sides
%%  (as plotted by Charney et al.)

rx = 3:M-2; ry = 2:N-2;
subplot(2,2,1) 
contour(X(rx,ry),Y(rx,ry),z0(rx,ry),zcontours);
title('Initial Analysis')
subplot(2,2,2) 
contour(X(rx,ry),Y(rx,ry),z24(rx,ry),zcontours);
title('Verifying Analysis')
subplot(2,2,3) 
contour(X(rx,ry),Y(rx,ry),z24(rx,ry)-z0(rx,ry),21);
title('Analysed Change')
pause

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Plot the analysis on the inner region
%%  omit one row at bottom and two rows
%%  on the other three sides.
clf;    %    Clear the plot window.
rx = 3:M-2; ry = 2:N-2;
contour(X(rx,ry),Y(rx,ry),z0(rx,ry),zcontours);
title('INITIAL HEIGHT (inner area)')
hold on
rx = 3:M-2; ry = 2:N-4;
loncontours = linspace(-180,+180,37);
latcontours = linspace(0,90,10);
contour(X(rx,ry),Y(rx,ry),LONDEG(rx,ry),loncontours);
contour(X(rx,ry),Y(rx,ry),LATDEG(rx,ry),latcontours);
hold off
pause

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Define working arrays to have correct size
ddxz    = zeros(M,N);     %  x derivative of z
ddyz    = zeros(M,N);     %  y derivative of z
gradzsq = zeros(M,N);     %  squared gradiant of z
d2dx2z  = zeros(M,N);     %  second x derivative of z
d2dy2z  = zeros(M,N);     %  second y derivative of z
xi      = zeros(M,N);     %  Laplacian of z
eta     = zeros(M,N);     %  absolute vorticity
ddxeta  = zeros(M,N);     %  x derivative of eta
ddyeta  = zeros(M,N);     %  y derivative of eta
Jacobi  = zeros(M,N);     %  Jacobian J(eta,z)
ddtz    = zeros(M,N);     %  Tendency of z
ddtxi   = zeros(M,N);     %  Tendency of xi
zdot    = zeros(M-2,N-2); %  Interior values of ddtz;
xidot   = zeros(M-2,N-2); %  Interior values of ddtxi;
ZDOT    = zeros(M-2,N-2); %  Fourier transform of zdot;
XIDOT   = zeros(M-2,N-2); %  Fourier transform of xidot;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Install the initial conditions for the forecast

if(StreamFunction==1)
  %  z0    = grav * z0 ./ FCOR;  %  Initial streamfunction
  z0  = grav * z0  / f0;  %  Initial streamfunction
end

z = z0;   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Compute the Laplacian of the height/psi
% Second x-derivative of z
d2dx2z(2:M-1,:) = (z(3:M,:)+z(1:M-2,:)-2*z(2:M-1,:))/(Ds^2);
% Second y-derivative of z
d2dy2z(:,2:N-1) = (z(:,3:N)+z(:,1:N-2)-2*z(:,2:N-1))/(Ds^2);

%%  Laplacian of height (or vorticity)
xi0(2:M-1,2:N-1) = d2dx2z(2:M-1,2:N-1)+d2dy2z(2:M-1,2:N-1);

%%  Extend xi0 to boundaries (for Jacobian).
xi0(1,:) = 2*xi0(2  ,:)-xi0(3  ,:);  %  West
xi0(M,:) = 2*xi0(M-1,:)-xi0(M-2,:);  %  East
xi0(:,1) = 2*xi0(:,2  )-xi0(:,3  );  %  South
xi0(:,N) = 2*xi0(:,N-1)-xi0(:,N-2);  %  North

%%  Absolute vorticity
eta0 = h.*xi0 + FCOR;

%% Compute statistics of initial fields
zmin = min(min(z0));
zave = mean(mean(z0));
zmax = max(max(z0));
fprintf(' z0:  min, ave, max %g %g %g \n',  zmin,  zave,  zmax)

ximin = min(min(xi0));
xiave = mean(mean(xi0));
ximax = max(max(xi0));
fprintf(' xi0: min, ave, max %g %g %g \n', ximin, xiave, ximax)

etamin = min(min(eta0));
etaave = mean(mean(eta0));
etamax = max(max(eta0));
fprintf('eta0: min, ave, max %g %g %g \n',etamin,etaave,etamax)

%  Plot the initial height, Laplacian and Vorticity
subplot(2,2,1)
contourf(X,Y,z0,21);
title('INITIAL HEIGHT/STREAMFUNCTION');
subplot(2,2,2)
contourf(X,Y,xi0,21);
title('INITIAL LAPLACIAN OF HEIGHT/PSI');
subplot(2,2,3)
contourf(X,Y,eta0,21);
title('INITIAL ABSOLUTE VORTICITY');
subplot(2,2,4)
contourf(X,Y,h); 
title('FACTOR h');
fprintf('Press RETURN to continue \n');
pause;   
clf;  % Clear the plot window.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%      MAIN LOOP     %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t1elapsed = clock;   %  Starting time
t1cpu = cputime;  %  For CPU time.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                           %%%
%%%      Integrate the BVE in time            %%%
%%%                                           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   Time-stepping is by leapfrog method
%%%   (first step is forward)
%%%
%%%   Define xi = Del^2(z). The BVE is
%%%
%%%       (d/dt)xi = J( h*xi+f, z)
%%%
%%%   We approximate the time derivative by
%%%     ( xi(n+1)-xi(n-1) ) / (2*Dt)    
%%%
%%%   The Jacobian term J(eta,z) is approximated
%%%   by centered space differences
%%%
%%%   Then the value of xi at the new time (n+1)*Dt is:
%%%      xi(n+1) =  xi(n-1) + 2*Dt * J(n)   
%%%
%%%   When we have ddtxi, we have to solve a Poisson
%%%   equation to get ddtz. Then both xi and z are stepped
%%%   forward to (n+1)*Dt and the cycle is repeated.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fprintf('  Total Number of Steps: %i \n', nt)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Start of the time-stepping loop  %%%%%%%%%

for n=1:nt
   
   %  Print out tracking information.
      PrintStep = 0;
      if ( PrintStep == 1 )
         fprintf(' Step number, time(h), time(d) %i %g %g \n',  ...
                  n,n*Dt/3600,n*Dt/(3600*24));
      end
      
%%  First time through the loop:
   if(n==1)
     dt = Dt/2;       %  First step is forward
     znm1  = z0;      %  Copy initial height field
     xinm1 = xi0;     %  Copy initial vorticity field
   end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Compute the derivatives, Laplacian and Jacobian.

% x-derivative of z
ddxz(2:M-1,:) = (z(3:M,:)-z(1:M-2,:))/(2*Ds);

% y-derivative of z
ddyz(:,2:N-1) = (z(:,3:N)-z(:,1:N-2))/(2*Ds);

% Square of the gradient of z
gradzsq = ddxz.^2+ddyz.^2;

% Second x-derivative of z
d2dx2z(2:M-1,:) = (z(3:M,:)+z(1:M-2,:)-2*z(2:M-1,:))/(Ds^2);

% Second y-derivative of z
d2dy2z(:,2:N-1) = (z(:,3:N)+z(:,1:N-2)-2*z(:,2:N-1))/(Ds^2);

%%  Laplacian of height
%%  xi = d2dx2z + d2dy2z;
xi(2:M-1,2:N-1) = d2dx2z(2:M-1,2:N-1)+d2dy2z(2:M-1,2:N-1);

%%  Extend xi to boundaries (to compute Jacobian).
%%  (First time step only; xi from BCs after that)
if ( n == 1 )
   xi(1,:) = 2*xi(2  ,:)-xi(3  ,:);  %  West
   xi(M,:) = 2*xi(M-1,:)-xi(M-2,:);  %  East
   xi(:,1) = 2*xi(:,2  )-xi(:,3  );  %  South
   xi(:,N) = 2*xi(:,N-1)-xi(:,N-2);  %  North
end

%%  Absolute vorticity
eta = h.*xi + FCOR;

% x-derivative of eta
ddxeta(2:M-1,:) = (eta(3:M,:)-eta(1:M-2,:))/(2*Ds);

% y-derivative of eta
ddyeta(:,2:N-1) = (eta(:,3:N)-eta(:,1:N-2))/(2*Ds);

%%  Compute the Jacobian J(eta,z)
Jacobi = ddxeta .* ddyz - ddyeta .* ddxz;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Calculate the energy and enstrophy integrals
   E(n) = 0.5 * sum(sum(gradzsq));
   S(n) = 0.5 * sum(sum(xi.^2));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Solve the Poisson Equation Del^2(ddtz) = ddtxi
%%  with homogeneous boundary conditions:  z is 
%%  constant on the boundaries, so ddtz vanishes.


%% Note: Fourier transform of xidot denoted XIDOT
%%       Fourier transform of zdot  denoted ZDOT.
%%       Forward Fourier transform: 
%%          XIDOT = SM*xidot*SN;
%%       Inverse transform: 
%%          zdot = (4/((M-1)*(N-1)))*SM*ZDOT*SN;
%%

%  Tendency values in interior.
   xidot = Jacobi(2:M-1,2:N-1); 

%  Compute the transform of the solution
   XIDOT = SM*xidot*SN;

%  Convert transform of d(xi)/dt to transform of d(z)/dt
   ZDOT = XIDOT ./ EIGEN;
 
%  Compute inverse transform to get the height tendency.
   zdot = (4/((M-1)*(N-1))) * SM*ZDOT*SN;

   ddtz (2:M-1,2:N-1) = zdot;    %  Insert inner values 
   ddtxi(2:M-1,2:N-1) = xidot;   %  Insert inner values 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Compute ddtxi on the boundaries.
%%  If fluid is entering through the boundary,
%%  we set ddtxi to zero. If fluid is leaving 
%%  the region, we extrapolate ddtxi linearly
%%  from the interior. Charney, et al., Eqn (21).

%  Western boundary
sigW = max(0,+sign(ddyz(1,:)));
ddtxi(1,:) = sigW.*(2*ddtxi(2  ,:)-ddtxi(3  ,:)); 

%  Eastern boundary
sigE = max(0,-sign(ddyz(M,:)));
ddtxi(M,:) = sigE.*(2*ddtxi(M-1,:)-ddtxi(M-2,:));

%  Southern boundary
sigS = max(0,-sign(ddxz(:,1)));
ddtxi(:,1) = sigS.*(2*ddtxi(:,2  )-ddtxi(:,3  ));

%  Northern boundary
sigN = max(0,+sign(ddxz(:,N)));
ddtxi(:,N) = sigN.*(2*ddtxi(:,N-1)-ddtxi(:,N-2));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Step forward one time-step (leapfrog scheme).
   xinp1 = xinm1 + (2*dt) * ddtxi;
   znp1  = znm1  + (2*dt) * ddtz;

%  Move the old values into arrays znm1 and xinm1
%  Move the new values into arrays z and xi
   znm1 = z;  
   xinm1 = xi;  
   z  = znp1;  
   xi = xinp1;  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Save the fields at quarterpoints of the integration
   if(n==1*nt/4) zq1 = z; end
   if(n==2*nt/4) zq2 = z; end
   if(n==3*nt/4) zq3 = z; end
   if(n==4*nt/4) zq4 = z; end

%  Plot the height field at each time step (if required)
   plotsteps = 0;   %  Turn plotting off for timing.
   if(plotsteps==1)
     surf(X,Y,z);  shading('interp')
     axis('off'); view(0,90);
     drawnow; 
   end

%  Restore the timestep (after the first step)
   dt = Dt; 

end     %%%%%    End of the time-stepping loop 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Get the elapsed ime the integration

t2elapsed = clock;   %  Starting time
ElapsedTime = etime(t2elapsed,t1elapsed);  % Elapsed time
t2cpu = cputime;
CPUtime = t2cpu - t1cpu;  %  For CPU time.
fprintf(' Elapsed time %g seconds \n', ElapsedTime)
fprintf(' CPU time %g  seconds \n', CPUtime)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%     END MAIN LOOP    %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Calculate the energy and enstrophy
ddxz(2:M-1,:) = (z(3:M,:)-z(1:M-2,:))/(2*Ds);
ddyz(:,2:N-1) = (z(:,3:N)-z(:,1:N-2))/(2*Ds);
gradzsq = ddxz.^2+ddyz.^2;
E(nt+1) = 0.5 * sum(sum(gradzsq));
S(nt+1) = 0.5 * sum(sum(xi.^2));

%  Plot the Energy and Enstrophy Integrals.
plotnn=88;     %   Indicator for plotting
if(plotnn==88)
  clf;    %    Clear the plot window.
  plot(time,E); title('Total Energy');
  axis([0,time(nt),0,2*E(1)]);
  fprintf('Press RETURN to continue \n'); pause
  plot(time,S); title('Total Enstrophy'); 
  axis([0,time(nt),0,2*S(1)]);
  fprintf('Press RETURN to continue \n'); pause
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Convert back from psi to z if necessary
if(StreamFunction==1)
  z0   = f0 * z0  / grav;  %  Initial streamfunction
  z    = f0 * z   / grav;  %  Initial streamfunction
  xi   = f0 * xi  / grav;  %  Initial Laplacian
  zq1  = f0 * zq1 / grav;  %  Solution at quarter-point
  zq2  = f0 * zq2 / grav;  %  Solution at quarter-point
  zq3  = f0 * zq3 / grav;  %  Solution at quarter-point
  zq4  = f0 * zq4 / grav;  %  Solution at quarter-point
end

%  Plot the final height field
contourf(X,Y,z,zcontours)
title('FORECAST HEIGHT FIELD');
fprintf('Press RETURN to continue \n'); 
pause
hold off

%  Plot the final height, laplacian and vorticity
subplot(2,2,1)
contourf(X,Y,z,zcontours);
title('FORECAST GEOPOTENTIAL HEIGHT');
subplot(2,2,2)
contourf(X,Y,xi,21);
title('FORECAST LAPLACIAN OF HEIGHT');
subplot(2,2,3)
contourf(X,Y,eta,21);
title('FORECAST ABSOLUTE VORTICITY');
fprintf('Press RETURN to continue \n');
pause;   clf;  % Clear the plot window.

%%  Plot the heights at check-points.
rx = 1:M  ; ry = 1:N  ;   %  Full area
subplot(2,2,1) 
contour(X(rx,ry),Y(rx,ry),zq1(rx,ry),zcontours);
title('Z (6 hours)')
subplot(2,2,2) 
contour(X(rx,ry),Y(rx,ry),zq2(rx,ry),zcontours);
title('Z (12 hours)')
subplot(2,2,3) 
contour(X(rx,ry),Y(rx,ry),zq3(rx,ry),zcontours);
title('Z (18 hours)')
subplot(2,2,4) 
contour(X(rx,ry),Y(rx,ry),zq4(rx,ry),zcontours);
title('Z (24 hours)')
pause

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Plot inner region: omit one row at bottom
%%  and two rows on the other three sides.
%%  These figures are in the same form as
%%  Figs 2 to 5 in Charney et al. (1950)

rx = 3:M-2; ry = 2:N-2;
subplot(2,2,1) 
contour(X(rx,ry),Y(rx,ry),z0(rx,ry),zcontours,'b');
title('(A) INITIAL ANALYSIS')
axis off; hold on;
plot([3 M-2 M-2 3 3],[2 2 N-2 N-2 2],'k');

subplot(2,2,2) 
contour(X(rx,ry),Y(rx,ry),z24(rx,ry),zcontours,'b');
title('(B) VERIFYING ANALYSIS')
axis off; hold on;
plot([3 M-2 M-2 3 3],[2 2 N-2 N-2 2],'k');

subplot(2,2,3) 
contour(X(rx,ry),Y(rx,ry),z24(rx,ry)-z0(rx,ry),11,'b-');
hold on
contour(X(rx,ry),Y(rx,ry),z(rx,ry)-z0(rx,ry),11,'r--');
title('(C) ANALYSED & FORECAST CHANGES')
axis off; hold on;
plot([3 M-2 M-2 3 3],[2 2 N-2 N-2 2],'k');

subplot(2,2,4) 
contour(X(rx,ry),Y(rx,ry),z(rx,ry),zcontours,'b');
title('(D) FORECAST HEIGHT')
axis off; hold on;
plot([3 M-2 M-2 3 3],[2 2 N-2 N-2 2],'k');
pause

%%  Save the four-panel image.
PrintFiles = 1;
if ( PrintFiles == 1 )
  print -depsc 'NEW-Case-N.eps'
  print -djpeg 'NEW-Case-N.jpg'
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Calculate the Forecast & Persistence BIAS
sum0 = 0; sum1 = 0; sum2 = 0; sum3 = 0;
rx = 3:M-2; ry = 2:N-2;
for nx=rx
for ny=ry
  sum0 = sum0 + 1;
  sum1 = sum1 + ( z(nx,ny)-z24(nx,ny));
  sum2 = sum2 + (z0(nx,ny)-z24(nx,ny));
  sum3 = sum3 + ( z(nx,ny)- z0(nx,ny));
end
end
rms1 = (sum1/sum0);
rms2 = (sum2/sum0);
rms3 = (sum3/sum0);
fprintf(' Bias of 24h forecast: %g \n', rms1);
fprintf(' Bias of persistence:  %g \n', rms2);
fprintf(' Mean 24h forecast change:   %g \n', rms3);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Calculate the Forecast & Persistence RMS Errors
sum0 = 0; sum1 = 0; sum2 = 0; sum3 = 0;
rx = 3:M-2; ry = 2:N-2;
for nx=rx
for ny=ry
  sum0 = sum0 + 1;
  sum1 = sum1 + ( z(nx,ny)-z24(nx,ny))^2;
  sum2 = sum2 + (z0(nx,ny)-z24(nx,ny))^2;
  sum3 = sum3 + ( z(nx,ny)- z0(nx,ny))^2;
end
end
rms1 = sqrt(sum1/sum0);
rms2 = sqrt(sum2/sum0);
rms3 = sqrt(sum3/sum0);
fprintf(' RMS error of 24h forecast: %g \n', rms1);
fprintf(' RMS error of persistence:  %g \n', rms2);
fprintf(' RMS 24h forecast change:   %g \n', rms3);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Compute the S1 Scores for the  Forecast and
%%  for a persistence forecast

ddxz(2:M-1,:) = (z(3:M,:)-z(1:M-2,:))/(2*Ds);
ddyz(:,2:N-1) = (z(:,3:N)-z(:,1:N-2))/(2*Ds);
gradzsq = ddxz.^2+ddyz.^2;
sgradz = sum(sum(sqrt(gradzsq)));

ddxz(2:M-1,:) = (z0(3:M,:)-z0(1:M-2,:))/(2*Ds);
ddyz(:,2:N-1) = (z0(:,3:N)-z0(:,1:N-2))/(2*Ds);
gradzsq = ddxz.^2+ddyz.^2;
sgradz0 = sum(sum(sqrt(gradzsq)));

ddxz(2:M-1,:) = (z24(3:M,:)-z24(1:M-2,:))/(2*Ds);
ddyz(:,2:N-1) = (z24(:,3:N)-z24(:,1:N-2))/(2*Ds);
gradzsq = ddxz.^2+ddyz.^2;
sgradz24 = sum(sum(sqrt(gradzsq)));

ddxz(2:M-1,:) = (  z(3:M,:)-  z(1:M-2,:))/(2*Ds) ...
               -(z24(3:M,:)-z24(1:M-2,:))/(2*Ds);
ddyz(:,2:N-1) = (  z(:,3:N)-  z(:,1:N-2))/(2*Ds) ...
               -(z24(:,3:N)-z24(:,1:N-2))/(2*Ds);
gradzsq = ddxz.^2+ddyz.^2;
sgradzmz24 = sum(sum(sqrt(gradzsq)));
S1fcst = (sgradzmz24/max(sgradz,sgradz24)) * 100;

ddxz(2:M-1,:) = ( z0(3:M,:)- z0(1:M-2,:))/(2*Ds) ...
               -(z24(3:M,:)-z24(1:M-2,:))/(2*Ds);
ddyz(:,2:N-1) = ( z0(:,3:N)- z0(:,1:N-2))/(2*Ds) ...
               -(z24(:,3:N)-z24(:,1:N-2))/(2*Ds);
gradzsq = ddxz.^2+ddyz.^2;
sgradz0mz24 = sum(sum(sqrt(gradzsq)));
S1Pers = (sgradz0mz24/max(sgradz0,sgradz24)) * 100;

fprintf(' S1 score for 24h forecast: %g \n', S1fcst);
fprintf(' S1 score for persistence:  %g \n', S1Pers);
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

