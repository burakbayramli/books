function [r,s,t] = abctorst(a,b,c)
  
% function [r,s,t] = abctorst(a,b,c)
% Purpose: Transfer from (a,b,c) -> (r,s,t) coordinates in triangle  

r = 0.25*(1+a).*(1-b).*(1-c)-1; s = 0.5*(1+b).*(1-c)-1; t = c;
return
