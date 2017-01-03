%nm2e04
clear
N=4; kmax=100; tol=10^-6;
At=[0 1 0 0; 1 0 1 0; 0 1 0 1; 0 0 1 0]/2;
x0=0; x5=10; %boundary values
b=[x0/2 0 0 x5/2]';
%initialize all the values to the average of boundary values
xp=ones(N,1)*(x0+x5)/2; 
%Jacobi iteration
for k=1:kmax
   x= At*xp +b; %Eq.(E2.4)
   if norm(x-xp)/(norm(xp)+eps)<tol, break; end
   xp=x;
end
k, xj=x
%Gauss-Seidal iteration
xp=ones(N,1)*(x0+x5)/2; x=xp; %initial value
for k=1:kmax
   for n=1:N, x(n)= At(n,:)*x +b(n); end %Eq.(E2.4)
   if norm(x-xp)/(norm(xp)+eps)<tol, break; end
   xp=x;
end
k, xg=x
