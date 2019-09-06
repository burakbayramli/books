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
