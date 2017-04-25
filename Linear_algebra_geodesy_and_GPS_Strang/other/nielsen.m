% format short g
% use analytical partial derivatives
partial = 'analytical';
%partial = 'n';
% speed of light, [m/s]
%clight = 300000000;
clight = 299792458;
% length of C/A code, [m]
%L = 300000;

% true position (Landmaalervej, Hjortekaer)
xtrue = [3507884.948 780492.718 5251780.403 0]';
% positions of satellites 1, 4, 7, 13, 20, 24 and 25 in ECEF coordinate system, [m]
xxyyzz = [16577402.072 5640460.750 20151933.185;
	  11793840.229 -10611621.371 21372809.480;
	  20141014.004 -17040472.264 2512131.115;
	  22622494.101 -4288365.463 13137555.567;
	  12867750.433 15820032.908 16952442.746;
	  -3189257.131 -17447568.373 20051400.790;
	  -7437756.358 13957664.984 21692377.935];

pseudorange = [20432524.0 21434024.4 24556171.0 21315100.2 21255217.0 ...
		       24441547.2 23768678.3]'; % [m]

l = pseudorange; % l is \ell (not one)
xx = xxyyzz(:,1);
yy = xxyyzz(:,2);
zz = xxyyzz(:,3);
n = size(xx,1); % number of observations

% weight matrix
sprior2 = 10^2; %5^2; %prior variance [m^2]
P = eye(n)/sprior2; % weight = 1/"prior variance" [m^(-2)]

% preliminary position, [m]
x = [0 0 0 0]';
p = size(x,1); % number of elements/parameters
f = n-p; % number of degrees of freedom
x0 = x;

for iter = 1:20 
  range = sqrt((x(1)-xx).^2+(x(2)-yy).^2+(x(3)-zz).^2);
  prange = range+x(4);
  F = prange;

  A = [];
  if strcmp(partial,'analytical')
		       % A is matrix of analytical partial derivatives
    irange = 1./range;
    dF = irange.*(x(1)-xx);
    A = [A dF];
    dF = irange.*(x(2)-yy);
    A = [A dF];
    dF = irange.*(x(3)-zz);
    A = [A dF];
    dF = ones(n,1);
    A = [A dF];

  else
			% A is matrix of numerical partial derivatives
    dF = sqrt((x(1)+1-xx).^2+(x(2) -yy).^2+(x(3) -zz).^2)+ x(4)  -prange;
    A = [A dF];
    dF = sqrt((x(1) -xx).^2+(x(2)+1-yy).^2+(x(3) -zz).^2)+ x(4)  -prange;
    A = [A dF];
    dF = sqrt((x(1) -xx).^2+(x(2) -yy).^2+(x(3)+1-zz).^2)+ x(4)  -prange; 
    A = [A dF];
    dF = sqrt((x(1)-xx).^2+(x(2)-yy).^2+(x(3)-zz).^2)+(x(4)+1)-prange;
    A = [A dF];
  end

  k = l-F; % l is \ell (not one)
	   %k = -l+F;
  N = A'*P;
  c = N*k;
  N = N*A;
  deltahat = N\c;
				% OLS solution
				%deltahat = A\k;
				% WLS-as-OLS solution
				%sqrtP = sqrt(P);
				%deltahat = (sqrtP*A)\(sqrtP*k)

  khat = A*deltahat;
  vhat = k-khat;
				% prepare for iterations
  x = x+deltahat;
				% stop iterations
  if max(abs(deltahat))<0.001
    break
  end
				%itertst = (k'*P*k)/(vhat'*P*vhat);
				%if itertst < 1.000001
				%
  break
				%end

end

% DOP
SSE = vhat'*P*vhat; %RSS or SSE
s02 = SSE/f; % MSE
s0 = sqrt(s02); %RMSE
Qdop = inv(A'*P*A);
Qx = s02.*Qdop;
Qdop = Qdop/sprior2;
PDOP = sqrt(trace(Qdop(1:3,1:3)));
% must be in local Easting-Northing-Up coordinates
%HDOP = sqrt(trace(Qdop(1:2,1:2)));
% must be in local Easting-Northing-Up coordinates
%VDOP = sqrt(Qdop(3,3));
TDOP = sqrt(Qdop(4,4));
GDOP = sqrt(trace(Qdop));

% Dispersion etc of elements
%Qx = s02.*inv(A'*P*A);
sigmas = sqrt(diag(Qx));
sigma = diag(sigmas);
isigma = inv(sigma);
% correlations between estimates
Rx = isigma*Qx*isigma;

% Standardised residuals
%Qv = s02.*(inv(P)-A*inv(A'*P*A)*A');
Qv = s02.*inv(P)-A*Qx*A';
sigmares = sqrt(diag(Qv));
stdres = vhat./sigmares;

disp('----------------------------------------------------------')
disp('estimated parameters/elements [m]')
x
disp('estimated clock error [s]')
x(4)/clight
disp('number of iterations')
iter
disp('standard errors of elements [m]')
sigmas
%tval = x./sigmas
disp('s0')

s0
disp('PDOP')
PDOP
%stdres
disp('difference between estimated elements and initial guess')
deltaori = x-x0
disp('difference between true values and estimated elements')
deltaori = xtrue-x
disp('----------------------------------------------------------')

% t-values and probabilities of finding larger |t|
% pt should be smaller than, say, (5% or) 1%
t = x./sigmas;
pt = betainc(f./(f+t.^2),0.5*f,0.5);
% probabilitiy of finding larger s02
% should be greater than, say, 5% (or 1%)
pchi2 = 1-gammainc(0.5*SSE,0.5*f);
% semi-axes in confidence ellipsoid for position estimates
% 95% fractile for 3 dfs is 7.815 = 2.796^2
% 99% fractile for 3 dfs is 11.342 = 3.368^2
[vQx dQx] = eigsort(Qx(1:3,1:3));
semiaxes = sqrt(diag(dQx));
% 95% fractile for 2 dfs is 5.991 = 2.448^2
% 99% fractile for 2 dfs is 9.210 = 3.035^2

