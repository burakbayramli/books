% Program for solving 2PBVP for a second-order system, y''=f(t,y,y'),
% with end conditions on dependent variable, y(t0)=a; y(tf)=b,
% by collocation of a quintic polynomial function
% y=c0+c1*t+c2*t^2+c3*t^3+c4*t^4+c5*t^5
% on single interval [t0,tf].
% Requires 'ode2ord.m' for specifying the differential equation
% for integration by MATLAB's RK-4(5) IVP code 'ode45.m'.
% (c) 2009 Ashish Tewari
% Solution vector: c=[c0, c1, c2, c3, c4, c5]';
function c=bvp2ordquintic(t0,tf,a,b)
tol=1e-6;
t1=(tf+t0)/2
y0dot=(b-a)/(tf-t0)
f0=t0-2*a+y0dot;
C1=[1 t0 t0^2 t0^3 t0^4 t0^5; 0 1 2*t0 3*t0^2 4*t0^3 5*t0^4;
0 0 2 6*t0 12*t0^2 20*t0^3; 0 0 2 6*tf 12*tf^2 20*tf^3;
0 1 2*tf 3*tf^2 4*tf^3 5*tf^4; 1 tf tf^2 tf^3 tf^4 tf^5];
[T,Y]=ode45('ode2ord',[t0 t1],[a y0dot]');
n=size(T,1); y1=Y(n,1); y1dot=Y(n,2); f1=t1-2*y1+y1dot;
[T,Y]=ode45('ode2ord',[t0 tf],[a y0dot]');
n=size(T,1); yf=Y(n,1); yfdot=Y(n,2); ff=tf-2*b+yfdot;
c=inv(C1)*[a y0dot f0 ff yfdot b]';
c0=c(1,1);c1=c(2,1);c2=c(3,1);c3=c(4,1);c4=c(5,1);c5=c(6,1);
err=f1-2*c2-6*c3*t1-12*c4*t1^2-20*c5*t1^3
while abs(err)>tol
y0dot=y0dot-err
f0=t0-2*a+y0dot;
[T,Y]=ode45('ode2ord',[t0 t1],[a y0dot]');
n=size(T,1); y1=Y(n,1); y1dot=Y(n,2); f1=t1-2*y1+y1dot;
[T,Y]=ode45('ode2ord',[t0 tf],[a y0dot]');
n=size(T,1); yf=Y(n,1); yfdot=Y(n,2); ff=tf-2*b+yfdot;
c=inv(C1)*[a y0dot f0 ff yfdot b]';
c0=c(1,1);c1=c(2,1);c2=c(3,1);c3=c(4,1);c4=c(5,1);c5=c(6,1);
err=f1-2*c2-6*c3*t1-12*c4*t1^2-20*c5*t1^3;
end
% Program for specifying the differential equation
% for integration by MATLAB's RK-4(5) IVP code 'ode45.m'.
function dydx=ode2ord(x,y)
dydx=[y(2)
x-2*y(1)+y(2)];
