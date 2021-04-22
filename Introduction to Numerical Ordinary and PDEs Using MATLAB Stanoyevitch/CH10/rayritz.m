function [x,u] = rayritz(p, q, f, n)
% This program will implement the piecewise linear Rayleigh-Ritz method to
% solve the BVP:  -(p(x)u'(x))'+q(x)u(x)=f(x),  u(0)=0,  u(1)=0.  The
% integral approximations (48) through (50) of Chapter 10 will be used.
% Input variables:  the first three:  p, q and f are inline functions
% representing the the DE, n = the number of interior x-grid values to
% employ.  A uniform grid is used.
% NOTE:  The program is set up to require that the functions p, q, and f
% take vector arguments.
% Output variables:  x and u are same sized vectors representing the x grid
% and the numerical solution values respectively

x=linspace(0,1,n+2);, h = x(2)-x(1);
% Use (48) and (49) of Chapter 10 to assemble diagonals of the symmetric
% tridiagonal stiffness matrix:
d = 1/(2*h)*(feval(p, x(1:n))+2*feval(p, x(2:n+1))+feval(p, x(3:n+2)))+...
    h/12*(feval(q, x(1:n))+6*feval(q, x(2:n+1))+feval(q, x(3:n+2)));
% for off diagonals
offdiag= -1/(2*h)*(feval(p,x(2:n))+feval(p,x(3:n+1)))+...
    h/12*(feval(q,x(2:n))+feval(q,x(3:n+1)));
% by symmtetry and to conform to syntax of 'thomas.m'
da = [offdiag 0]; %above diagonal
db = [0 offdiag]; %below diagonal

% Use (50) of Chapter 10 to construct vector b
b = h/6*(feval(f,x(1:n))+4*feval(f,x(2:n+1))+feval(f,x(3:n+2)));

% Use the Thomas method to solve the system Au=b and get solution
u = thomas(da,d,db,b);
u = [0 u 0]; %adjoin boundary values

