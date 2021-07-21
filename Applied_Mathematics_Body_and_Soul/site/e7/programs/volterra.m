function Up = volterra(t, U)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The Volterra-Lotka equations, also known as the 
% prey-predator equations, are a pair of first order, 
% non-linear, differential equations frequently used 
% to describe the dynamics of biological systems.
%
% du1/dt = u1(a - bu2)
% du2/dt = -u2(c - du1)
%
%    u1 is the number of some prey (for example, rabbits); 
%    u2 is the number of its predator (for example, foxes); 
%    t represents the development of the two populations against time; 
%    and a, b, c and d are parameters respresenting the 
%    interaction of the two species.
%
% The prey are supposed to have unlimited food and to reproduce 
% indefinitely unless subject to predation. The predators thrive when 
% there are plentiful prey but, ultimately, outstrip their food 
% supply and decline.
%
% The equations have periodic solutions which do not have a 
% simple expression in terms of the usual trigonometric functions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  a = 1;
  b = 0.1;
  c = 0.5;
  d = 0.02;
  
  Up(1) = U(1)*(a - b* U(2));  Up(2) = -U(2)*(c - d*U(1));


