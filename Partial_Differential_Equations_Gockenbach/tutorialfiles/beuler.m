function [a,t]=beuler(M,K,f,a0,N,dt)

%[a,t]=beuler(M,K,f,a0,N,dt)
%
%   This function applies the backward Euler method to solve the
%   IVP
%
%       Ma'+Ka=f(t),
%       a(0)=a0,
%
%   where M and K are mxm matrices (with M invertible) and f is
%   a vector-valued function taking two arguments, t and m.
%
%   N steps are taken, each of length dt.

m=length(a0);
a=zeros(m,N+1);
a(:,1)=a0;
t=linspace(0,N*dt,N+1)';

L=M+dt*K;
for ii=1:N
   a(:,ii+1)=L\(M*a(:,ii)+dt*f(t(ii+1),m));
end

