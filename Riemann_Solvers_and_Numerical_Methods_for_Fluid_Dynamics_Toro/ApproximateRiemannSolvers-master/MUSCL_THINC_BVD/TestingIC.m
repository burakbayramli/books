function u0 = TestingIC(x)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%         Construct a testing Initial Condition for any FDM and DGM.
%
%               coded by Manuel Diaz, NTU, 2013.12.20.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Taken from:
% L. Krivodonova; Limiters for high-order Discontinuous Galerkin Methods,
% JCP 226 (2007) 897-896. DOI 10.1016/j.jcp.2007.05.011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTES:
% This IC is intended to be used for any x array inside range [-1,1] only!
% e.g.: x = -1:0.01:1;

% x-ranges
x1 = x.*(x>=-0.8 & x<=-0.6);
x2 = x.*(x>=-0.4 & x<=-0.2);
x3 = x.*(x>= 0.0 & x<= 0.2);
x4 = x.*(x>= 0.4 & x<= 0.6);

% parameters
a=0.5; z=-0.7; delta=0.005; alpha=10; beta=log(2)/(36*delta^2);

% functions
G = @(x,b,r) exp(-b*(x-r).^2);
F = @(x,d,r) sqrt(max(1-d^2*(x-r).^2,0));

% Testing Initial Condition
u0 = 1/6*( G(x1,beta,z-delta) + G(x1,beta,z+delta) + 4*G(x1,beta,z)) + ...
     1*(x2~=0) + ...
     1-abs(10*(x3-0.1)) + ...
     1/6*(F(x4,alpha,a-delta) + F(x4,alpha,a+delta) + 4*F(x4,alpha,a));

% plot (just for testing this implementation)
%plot(x,u0,'-k');
 
end