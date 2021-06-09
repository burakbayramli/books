function y = EX13_7_poisson(phi,r,th)
for i = 1:length(phi)
if (0<=phi(i))&(phi(i)<=2)
     y(i)=2*phi(i)^2*(1-r^2)/2/pi/(1-2*r*cos(th-phi(i))+r^2);
elseif (2< phi(i))&( phi(i)<=3)
     y(i)=8*(1-r^2)/2/pi/(1-2*r*cos(th-phi(i))+r^2);
else
     y(i)=0;
end
end
