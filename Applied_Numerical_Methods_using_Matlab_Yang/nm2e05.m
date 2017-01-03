%nm2e05.m
% use Gauss-Seidal iteration to solve a set of nonlinear equations
clear
kmax=100; tol=1e-6;
x=zeros(1,2); %initial value 
for k=1:kmax
   xp=x; % to remember the previous solution 
   x(1)= (13-x(1)^2-2*x(2)^2)/10;
   x(2)= (6-x(1)^3)/5;
   if norm(x-xp)/(norm(xp)+eps)<tol, break; end
end
k, x
