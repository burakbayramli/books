function rre_mm
%
% ODE15s solution the Reaction Rate Equation for 
% the Michaelis-Menten system.
%
% Parameters from Chapter 7 of 
%      Stochastic Modelling for Systems Biology,
%      by Darren J. Wilkinson, Chapman & Hall/CRC, 2006.
%
% Downloadable from 
%    http://www.maths.strath.ac.uk/~aas96106/algfiles.html
% along with an extended version that produces graphical output.

tspan = [0 50]; yzero = [5e-7; 2e-7; 0; 0];
options = odeset('AbsTol',1e-8);
k1 = 1e6; k2 = 1e-4; k3 = 0.1;
  
[t,y] = ode15s(@mm_rre,tspan,yzero,options);

% Record or plot (t,y) at this stage

      %--------------Nested function----------
      function yprime = mm_rre(t,y)
      % MM_RRE    Michaelis-Menten Reaction Rate Equation 
      yprime = zeros(4,1);
      yprime(1) = -k1*y(1)*y(2) + k2*y(3);
      yprime(2) = -k1*y(1)*y(2) + (k2+k3)*y(3);
      yprime(3) =  k1*y(1)*y(2) - (k2+k3)*y(3);
      yprime(4) =  k3*y(3);
      end

end





