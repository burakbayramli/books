function bc = specific_bc(xbd,ybd)
%solution1_bc   Reference problem 3.1  boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%   definition of 1-D analytic convection-diffusion boundary condition
%   requires viscosity to be a globally declared variable
%   IFISS function: DJS; 5 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
global viscosity
bc=xbd.*(1-exp((ybd-1)/viscosity))/(1-exp(-2/viscosity));
return