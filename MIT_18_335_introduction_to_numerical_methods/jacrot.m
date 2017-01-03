function J=jacrot(a,b,d)

tau=(b-a)/2/d;
t=sign(tau)/(abs(tau)+sqrt(1+tau^2));
c=1/sqrt(1+t^2);
s=c*t;
J=[c,s; -s,c];
