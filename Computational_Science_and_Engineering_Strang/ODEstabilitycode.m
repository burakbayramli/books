%6.2  ODEstabilitycode.m

% Code from Nick Trefethen
% p25.m - stability regions for ODE formulas

% Adams-Bashforth:
  clf, subplot('position',[.1 .56 .38 .38])
  plot([-8 8],[0 0]), hold on, plot([0 0],[-8 8])
  z = exp(1i*pi*(0:200)/100); r = z-1;
  s = 1; plot(r./s)                                  % order 1
  s = (3-1./z)/2; plot(r./s)                         % order 2
  s = (23-16./z+5./z.^2)/12; plot(r./s)              % order 3
  axis([-2.5 .5 -1.5 1.5]), axis square, grid on
  title Adams-Bashforth

% Adams-Moulton:
  subplot('position',[.5 .56 .38 .38])
  plot([-8 8],[0 0]), hold on, plot([0 0],[-8 8])
  s = (5*z+8-1./z)/12; plot(r./s)                    % order 3
  s = (9*z+19-5./z+1./z.^2)/24; plot(r./s)           % order 4 
  s = (251*z+646-264./z+106./z.^2-19./z.^3)/720; plot(r./s)    % 5
  d = 1-1./z;
  s = 1-d/2-d.^2/12-d.^3/24-19*d.^4/720-3*d.^5/160; plot(d./s) % 6
  axis([-7 1 -4 4]), axis square, grid on, title Adams-Moulton

% Backward differentiation:
  subplot('position',[.1 .04 .38 .38])
  plot([-40 40],[0 0]), hold on, plot([0 0],[-40 40]) 
  r = 0; for i = 1:5, r = r+(d.^i)/i; plot(r), end   % orders 1-5
  axis([-15 35 -25 25]), axis square, grid on
  title('backward differentiation')

% Runge-Kutta:
  subplot('position',[.5 .04 .38 .38])
  plot([-8 8],[0 0]), hold on, plot([0 0],[-8 8])
  w = 0; W = w; for i = 2:length(z)                  % order 1
    w = w-(1+w-z(i)); W = [W; w]; end, plot(W)
  w = 0; W = w; for i = 2:length(z)                  % order 2
    w = w-(1+w+.5*w^2-z(i)^2)/(1+w); W = [W; w];
    end, plot(W)
  w = 0; W = w; for i = 2:length(z)                  % order 3
    w = w-(1+w+.5*w^2+w^3/6-z(i)^3)/(1+w+w^2/2); W = [W; w];
    end, plot(W)
  w = 0; W = w; for i = 2:length(z)                  % order 4
    w = w-(1+w+.5*w^2+w^3/6+w.^4/24-z(i)^4)/(1+w+w^2/2+w.^3/6);
    W = [W; w]; end, plot(W)
  axis([-5 2 -3.5 3.5]), axis square, grid on, title Runge-Kutta
