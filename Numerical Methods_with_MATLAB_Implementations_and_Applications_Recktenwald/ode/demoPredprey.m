function demoPredprey(p1start,p2start,tstop)
% demoPredprey   Coupled ODEs for a two-species predator-prey simulation
%
% Synopsis:  demoPredprey
%            demoPredprey(p1start,p2start,tstop)
%
% Input:     pstart1 = (optional) initial population of species 1
%            pstart2 = (optional) initial population of species 2
%            tstop   = (optional) duration of the simulation
%
% Output:    Plot of populations versus time

if nargin<1,  p1start = 5000;  end
if nargin<2,  p2start = 100;   end
if nargin<3,  tstop = 30;      end

% ---  Parameters to be passed to rhspop2
alpha = [ 2.0  0.0002 ];      delta = [ 0.02  0.8 ];

p0 = [p1start; p2start];                           %  Initial conditions
[t,p] = ode45('rhspop2',tstop,p0,[],alpha,delta);  %  Solution with ode45
% [t,p] = rk4sysv('rhspop2',tstop,tstop/500,p0,alpha,delta);   % with rk4sysv

% --- Plot the results
subplot(2,1,1);   plot(t,p(:,1));    grid;   ylabel('Prey population');
title(sprintf('alpha(1) = %f    delta(1) = %f',alpha(1),delta(1)));

subplot(2,1,2);   plot(t,p(:,2));    grid;
xlabel('time (arbitrary units)');    ylabel('Predator population');
title(sprintf('alpha(2) = %f    delta(2) = %f',alpha(2),delta(2)));
