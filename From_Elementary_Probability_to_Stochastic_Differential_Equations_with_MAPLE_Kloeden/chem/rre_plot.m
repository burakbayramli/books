function rre_plot
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
% This is an extended version of rre_mm 
% that produces graphical output.

clf

tspan = [0 50]; yzero = [5e-7; 2e-7; 0; 0];
options = odeset('AbsTol',1e-8);
k1 = 1e6; k2 = 1e-4; k3 = 0.1;
  
[t,y] = ode15s(@mm_rre,tspan,yzero,options);

plot(t,y(:,1),'go-','MarkerSize',10)
hold on
plot(t,y(:,4),'r*-','MarkerSize',10)

text(33,3.5e-7,'Product','FontSize',16)
text(33,0.5e-7,'Substrate','FontSize',16)

xlabel('Time','FontSize',14)
ylabel('Concentration','FontSize',14)

set(gca,'FontWeight','Bold','FontSize',12)
grid on

      %--------------Nested function----------
      function yprime = mm_rre(t,y)
      % MM_RRE    Michaelis-Menten reaction Rate Equation 
      yprime = zeros(4,1);
      yprime(1) = -k1*y(1)*y(2) + k2*y(3);
      yprime(2) = -k1*y(1)*y(2) + (k2+k3)*y(3);
      yprime(3) =  k1*y(1)*y(2) - (k2+k3)*y(3);
      yprime(4) =  k3*y(3);
      end

end



