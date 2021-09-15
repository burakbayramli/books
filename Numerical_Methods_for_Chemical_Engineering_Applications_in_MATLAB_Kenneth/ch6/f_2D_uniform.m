% f_2D_uniform.m
% This routine returns a uniform value of f0 for all
% values of (x,y).
% K. Beers. MIT ChE. 9/27/03
function F = f_2D_uniform(X,Y,L,H);

f0 = 1;

F = f0*ones(size(X));

return;
