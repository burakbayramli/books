% PURPOSE: An example of using lndetfull, lndetmc, lndetint
%          to compute the log-determinant term
%          NOTE: these are usually
%                used inside the estimation functions  
% based on a larger data set, see lndet_d for a small dataset
%---------------------------------------------------
% USAGE: lndet_d2
%---------------------------------------------------

clear all;
load elect.dat;             % load data on votes
latt = elect(:,5);
long = elect(:,6);
clear elect; % conserve on RAM memory
n = 3107;

[j1 W j2] = xy2cont(latt,long); % weight matrix

% find eigenvalues min,max
% t0 = clock;
% opt.tol = 1e-3; opt.disp = 0;
% lambda = eigs(sparse(W),speye(n),1,'SR',opt);  
% rmin = 1/lambda;   
% rmax = 1;
% time1 = etime(clock,t0);

rmin = -1;
rmax = 0.99;

% use Pace and Barry 1997 sparse cholesky approach
t0 = clock;
out = lndetfull(W,rmin,rmax);
time2 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval1 = [tt' outi];

% use Pace and Barry, 1999 MC approximation
order = 50; iter = 30; % defaults
t0 = clock;
out = lndetmc(order,iter,W,rmin,rmax);
time3 = etime(clock,t0);
tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval2 = [tt' outi];


% % use Pace and Barry, 1998 spline interpolation
% t0 = clock;
% out = lndetint(W,rmin,rmax);
% time4 = etime(clock,t0);

tt=rmin:.001:rmax; % interpolate a finer grid
outi = interp1(out.rho,out.lndet,tt','spline');
detval3 = [tt' outi];

% print out times taken
disp('time in seconds taken');
in.cnames = strvcat('lndetfull','lndetmc','lndetint');
mprint([time2 time3 time3],in);



% plot these for comparison
tt = detval1(:,1);
plot(tt,detval1(:,2),'-r',tt,detval2(:,2),'-b');
% ,tt,out.up95,'-g',tt,out.lo95);
legend('lndetfull','lndetmc');
