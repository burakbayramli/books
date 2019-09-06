% Program for updating the perifocal position
% and velocity in an elliptical orbit
% (c) 2009 Ashish Tewari
function [R,V]=trajellip(mu,t0,R0,V0,t)
  eps=1e-10;
  r0=norm(R0);
  v0=norm(V0);
  alpha=dot(R0,V0);
  H=cross(R0,V0);
  h=norm(H);
  p=h^2/mu;
  ecv=cross(V0,H)/mu-R0/r0;
  e=norm(ecv);
  ecth0=p/r0-1;
  esth0=norm(cross(ecv,R0))/r0;
  if abs(ecth0)>=eps;
    th0=atan(esth0/ecth0);
    if ecth0<0
      if esth0>=0;
	th0=th0+pi;
      end
    elseif esth0<0
      th0=th0+2*pi;
    end
  elseif esth0>=0
    th0=pi/2;
  else
    th0=3*pi/2;
  end
  ainv=-(v0^2)/mu+2/r0;
  a=1/ainv;
  n=sqrt(mu/a^3);
  E0=2*atan(sqrt((1-e)/(1+e))*tan(0.5*th0));
  tau=t0+(-E0+e*sin(E0))/n;
  M=n*(t-tau);
  E=kepler(e,M);
  r=a*(1-e*cos(E));
end
f=1+a*(cos(E-E0)-1)/r0;
g=a*alpha*(1-cos(E-E0))/mu+r0*sqrt(a/mu)*sin(E-E0);
fd=-sqrt(mu*a)*(sin(E-E0))/(r*r0);
gd=1+a*(cos(E-E0)-1)/r;
R=f*R0+g*V0;
V=fd*R0+gd*V0;
