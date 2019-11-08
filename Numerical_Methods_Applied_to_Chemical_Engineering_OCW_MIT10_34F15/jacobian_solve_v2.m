function [Jx,Jp] = jacobian_solve_v2(func,t,x,p)
% Function to compute finite difference approximation of Jacobian of f(t,x,p)
% with respect to vectors x and p at point(t,x,p). t is a scalar.
% Written by: Bill Green 10/14/2015  from earlier versions by Sean Kessler
% and S. Harwood.
% Inputs:
%           func = handle of the function that returns f(t,x,p)
%           x = Nx dimensional vector at which Jacobian is desired.
%           p = Np dimensional vector p at which Jacobian is desired
%           
% Outputs:
%           Jx = M x Nx matrix (M = length of output of func) for which 
%               Jx(i,j) is finite difference approximation of partial 
%               derivative of the i'th func with respect to the j'th x
%           Jp = M x Np matrix where Jp(i,k) is finite difference 
%               approximation of partial derivative of i'th element of f
%               with respect to the k'th element of p
%
% we use centered differences, and choose dx = 0.00001*|x| + 1e-12

fx = feval(func,t,x,p);
M = length(fx);
Nx = length(x);
Np = length(p);
Jx = zeros(M,Nx);
Jp = zeros(M,Np);
dx = 0.00001*abs(x)+1e-8;
dp = 0.00001*abs(p)+1e-8;
x_up=x;  x_down=x;
for idex=1:Nx
    x_up(idex) = x(idex) + dx(idex);
    x_down(idex)= x(idex)- dx(idex);
    Jx(:,idex) = (feval(func,t,x_up,p) - feval(func,t,x_down,p))/(2*dx(idex));
    x_up(idex) = x(idex); 
    x_down(idex)= x(idex);
end
p_up=p;  p_down=p;
for idex=1:Np
    p_up(idex) = p(idex) + dp(idex);
    p_down(idex)= p(idex)- dp(idex);
    Jp(:,idex) = (feval(func,t,x,p_up) - feval(func,t,x,p_down))/(2*dp(idex));
    p_up(idex) = p(idex) ;
    p_down(idex)= p(idex);
end


end