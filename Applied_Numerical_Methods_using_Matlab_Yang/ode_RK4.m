function [t,y]=ode_RK4(f,tspan,y0,N,varargin)
%Runge-Kutta method to solve vector differential equation y'(t)=f(t,y(t))
% for tspan=[t0,tf] and with the initial value y0 and N time steps
if nargin<4|N<=0, N=100; end
if nargin<3, y0=0; end
y0=y0(:)'; %make it a row vector
y=zeros(N+1,length(y0)); y(1,:)=y0;
h=(tspan(2)-tspan(1))/N;
t=tspan(1)+[0:N]'*h; 
h2=h/2;
for k=1:N
  %if y(k,1)<0, pause; end
  f1=h*feval(f,t(k),y(k,:),varargin{:}); f1=f1(:)'; %(6.3-2a)
  f2=h*feval(f,t(k)+h2,y(k,:)+f1/2,varargin{:}); f2=f2(:)';%(6.3-2b)
  f3=h*feval(f,t(k)+h2,y(k,:)+f2/2,varargin{:}); f3=f3(:)';%(6.3-2c)
  f4=h*feval(f,t(k)+h,y(k,:)+f3,varargin{:}); f4=f4(:)'; %(6.3-2d)
  y(k+1,:)= y(k,:) +(f1+2*(f2+f3)+f4)/6; %Eq.(6.3-1)
end
%for k=1:N
%   fk1= h*feval(f,t(k),y(k,:),varargin{:}); fk1=fk1(:)';
%   fk2= h*feval(f,t(k)+h2,y(k,:)+fk1/2,varargin{:}); fk2=fk2(:)';
%   fk3= h*feval(f,t(k)+h2,y(k,:)+fk2/2,varargin{:}); fk3=fk3(:)';
%   fk4= h*feval(f,t(k)+h,y(k,:)+fk3,varargin{:}); fk4=fk4(:)';
%   y(k+1,:)= y(k,:) +(fk1+2*(fk2+fk3)+fk4)/6;
%end

%Krc=0; %assume row-wise function
%if size(feval(f,tspan(1),y0,varargin{:}),1)>1, Krc=1; end
%for k=1:N
% if Krc==0
%   k1= h*feval(f,t(k),y(k,:),varargin{:});
%   k2= h*feval(f,t(k)+h/2,y(k,:)+k1/2,varargin{:});
%   k3= h*feval(f,t(k)+h/2,y(k,:)+k2/2,varargin{:});
%   k4= h*feval(f,t(k)+h,y(k,:)+k3,varargin{:});
% else
%   k1= h*feval(f,t(k),y(k,:),varargin{:})';
%   k2= h*feval(f,t(k)+h/2,y(k,:)+k1/2,varargin{:})';
%   k3= h*feval(f,t(k)+h/2,y(k,:)+k2/2,varargin{:})';
%   k4= h*feval(f,t(k)+h,y(k,:)+k3,varargin{:})';
% end
% y(k+1,:)= y(k,:) +(k1+2*(k2+k3)+k4)/6;
%end