function [a,p,V1]=lambert_hypergeom(mu,R1,R2,t12,id)
% Solution to Lambert's two-point boundary value problem in space navigation.
% mu: gravitational constant of central mass.
% R1,V1: initial position and velocity vectors in celestial frame.
% R2,V2: final position and velocity vectors in celestial frame.
% t12: transfer time.
% id: indicator of transfer direction (id=1:direct; -1:retrograde)
% (c) 2010 Ashish Tewari
r1=norm(R1); r2=norm(R2);
costh=dot(R1,R2)/(r1*r2); % cosine transfer angle
theta=acos(costh);
if costh<0
theta=2*pi-theta;
end
cosi=dot([0 0 1]',cross(R1,R2)/norm(cross(R1,R2)));
if id*cosi<0
theta=2*pi-theta;
end
c=norm(R2-R1);
s=0.5*(r1+r2+c);
am=s/2;
L=sqrt(r1*r2)*cos(theta/2)/s;
x=0.1;
y=sqrt(1-L^2*(1-x^2));
eta=y-L*x;
z=0.5*(1-L-x*eta);
n=1;
F=hypergeom(z);
tc=(4*eta^3*F/3+4*L*eta)/sqrt(mu/am^3);
% Newton's iteration for 'x' follows:
while abs(t12-tc)>1e-9
fx=4*eta^3*F/3+4*L*eta-t12*sqrt(mu/am^3);
G=hypergeomd(z);
Fd=(6-3*6*G/5)/(2*(1-z)*(1-z*6*G/5));
fxp=-2*eta^5*Fd/(3*y)-L*eta*(4*eta^2*F+4*L)/y;
dx=-fx/fxp;
x=x+dx;
n=n+1;
y=sqrt(1-L^2*(1-x^2));
eta=y-L*x;
z=0.5*(1-L-x*eta);
F=hypergeom(z);
tc=(4*eta^3*F/3+4*L*eta)/sqrt(mu/am^3);
end
a=am/(1-x^2);
p=r1*r2*(sin(0.5*theta))^2/(am*eta^2);
V1=sqrt(mu/am)*((2*L*am/r1-(L+x*eta))*R1/r1+ ...
sqrt(r2/r1)*sin(0.5*theta)*cross(cross(R1,R2) ...
/norm(cross(R1,R2)),R1/r1))/eta;
