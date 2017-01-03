function dpdt = rhsPop2(t,p,flag,alpha,delta)
% rhsPop2  Right hand sides of coupled ODEs for 2 species predator-prey system
%
% Synopis:  dpdt = rhsPop2(t,p,flag,alpha,delta)
%
% Input:    t = time. Not used in this m-file, but needed by ode45
%           p = vector (length 2) of populations of species 1 and 2
%           flag  = (not used) placeholder for compatibility with ode45
%           alpha = vector (length 2) of growth coefficients
%           delta = vector (length 2) of mortality coefficients
%
% Output:   dpdt = vector of dp/dt values
dpdt = [ alpha(1)*p(1)      - delta(1)*p(1)*p(2);
         alpha(2)*p(1)*p(2) - delta(2)*p(2);      ];
