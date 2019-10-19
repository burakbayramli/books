function [alpha, alphax, alphas] = steplength(x, s, Dx, Ds, eta)
% 
% syntax: [alpha, alphax, alphas] = steplength(x, s, Dx, Ds, eta)
% 
% given current iterate (x,s) and steps (Dx,Ds), compute steplengths
% that ensure that x + alphax*Dx>0 and s + alphas*Ds>0, and
% alpha = min(alphax,alphas). eta indicates the maximum fraction of
% step to the boundary (typical value: eta=.999)
 
  alphax = -1/min(min(Dx./x),-1); alphax = min(1, eta * alphax);
  alphas = -1/min(min(Ds./s),-1); alphas = min(1, eta * alphas);
  alpha = min(alphax, alphas);
  
