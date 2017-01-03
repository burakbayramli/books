function [t,x]=bvp2_fdf(a1,a0,u,t0,tf,x0,xf,N)  
% solve BVP2: x"+a1*x'+a0*x=u with x(t0)=x0, x(tf)=xf
%  by the finite difference method
h=(tf-t0)/N; h2=2*h*h;
t=t0+[0:N]'*h;
if ~isnumeric(a1), a1=a1(t(2:N)); %a1 supposed to be the name of a function of t
 elseif length(a1)==1, a1=a1*ones(N-1,1);
end
if ~isnumeric(a0), a0=a0(t(2:N)); %a0 supposed to be the name of a function of t
 elseif length(a0)==1, a0=a0*ones(N-1,1);
end
if ~isnumeric(u), u=u(t(2:N));
 elseif length(u)==1,  u=u*ones(N-1,1); %u supposed to be the name of a function of t
 else u=u(:);
end
A=zeros(N-1,N-1);  b=h2*u;
ha=h*a1(1); A(1,1:2)= [-4+h2*a0(1)  2+ha]; 
b(1)=b(1)+(ha-2)*x0;
for m=2:N-2
   ha= h*a1(m); A(m,m-1:m+1)= [2-ha  -4+h2*a0(m)  2+ha];
end
ha=h*a1(N-1); A(N-1,N-2:N-1)= [2-ha  -4+h2*a0(N-1)];
b(N-1)=b(N-1)-(ha+2)*xf;
x=[x0 trid(A,b)' xf]';
   