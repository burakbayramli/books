% Example of ADMM for Basis Pursuit 

n=60; m=20;
A=randn(m,n); 
b=rand(m,1); 

% ADMM algorithm for finding sparsest solution
niter=150;
x=zeros(n,1); z=zeros(n,1); u=zeros(n,1);
rob=zeros(niter,1);
alpha=1.0; mu=1.0;

uA=inv(A*A');
aux1=eye(n)-(A'*uA*A);
aux2=A'*uA*b;
for nn=1:niter,
   % x update
   x=(aux1*(z-u))+aux2;
   % z update
   zo=z;
   xe=(alpha*x)+((1-alpha)*zo);
   aux=xe+u; imu=1/mu;
   z=max(0,aux-imu)-max(0,-aux-imu); %shrinkage
   % u update
   u=u+(xe-z);
   % recording
   rob(nn)=norm(x,1);      
end   

% display

figure(1)
stem(x,'k');
title('the sparsest solution x');
xlabel('n samples');

figure(2)
plot(rob,'k');
title('evolution of norm(x,1)');
xlabel('n iter');