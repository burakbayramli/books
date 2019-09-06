% Program 'gravitygradient.m' for calculating
% the gravity-gradient matrix in a spherical
% gravity field.
% (c) 2009 Ashish Tewari
function G=gravitygradient(mu,r)
  x=r(1,1);
  y=r(2,1);
  z=r(3,1);
  rad=norm(r);
  G=mu*[3*x^2-rad^2 3*x*y 3*x*z;
	3*x*y 3*y^2-rad^2 3*y*z;
	3*x*z 3*y*z 3*z^2-rad^2]/rad^5;
  dtr=pi/180;
  a=14000;
  e=0.5;
  i=40*dtr;
  Om=290*dtr;
  w=100*dtr;
  tau=-12000;
  mu=398600.4;
  n=sqrt(mu/a^3);
  C=rotation(i,Om,w);
  E0=kepler(e,-n*tau)
  r0=a*(1-e*cos(E0))
  v0=sqrt(2*mu/r0-mu/a)
  R0=a*[cos(E0)-e sqrt(1-e^2)*sin(E0) 0]'
  V0=sqrt(mu*a)*[-sin(E0);sqrt(1-e^2)*cos(E0);0]/r0;
  T=0;
  x0=[-100 50 200 0 0 0]';
  for k=1:10;
    [R,V]=trajellip(mu,T,R0,V0,T+100);
    R0=R;V0=V;
    Rn=C*R;
    G=gravitygradient(mu,R);
    A=[zeros(3,3) eye(3);G zeros(3,3)];B=[zeros(3,3);eye(3)];
    [k,s,E]=lqr(A,B,1e-8*eye(6),eye(3));
    sys=ss(A-B*k,B,[eye(3) zeros(3,3)],zeros(3,3));
    [y,t,x]=initial(sys,x0,100);
    u=-x*k';
    plot(t+T,y),hold on
    T=T+100;
    x0=x(size(t,1),:);
  end
