% speed of light, [m/s]
clight = 299792458;

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

% preliminary position, [m]
x = [0 0 0 0]';
x0 = x;

for iter = 1:20 
  range = sqrt((x(1)-xx).^2+(x(2)-yy).^2+(x(3)-zz).^2);
  prange = range+x(4);
  F = prange;
  A = [];
  irange = 1./range;
  dF = irange.*(x(1)-xx);
  A = [A dF];
  dF = irange.*(x(2)-yy);
  A = [A dF];
  dF = irange.*(x(3)-zz);
  A = [A dF];
  dF = ones(n,1);
  A = [A dF];

  k = l-F; % l is \ell (not one)
  N = A';
  c = N*k;
  N = N*A;
  deltahat = N\c;
  x = x+deltahat;
  
  if max(abs(deltahat))<0.001
    break
  end


end

disp('----------------------------------------------------------')
disp('estimated parameters/elements [m]')
x
disp('estimated clock error [s]')
x(4)/clight
disp('iteration')
iter

