function [ K] = Kstif2Dtriangle( E,l,w,mu,x1,y1,x2,y2,x3,y3,b1,b2,b3,c1,c2,c3 )
  A=trianglearea(x1,y1,x2,y2,x3,y3);
  B=[b1 0 b2 0 b3 0; 0 c1 0 c2 0 c3; c1 b1 c2 b2 c3 b3];
  B=B/(2*A);
  d11=E/(1-mu^2);
  d22=E/(1-mu^2);
  d12=mu*E/(1-mu^2);
  d21=mu*E/(1-mu^2);
  d33=E/(2*(1+mu));
  D=[d11 d12 0; d21 d22 0;0 0 d33];
  K=B'*D*B;
  K=K*A;
end
