function yfex=exact_solution(range,y0,pid_parms);
% EXACT_SOLUTION
% Compute the "true" solution at the data points.
%
c=pid_parms(1);
k=pid_parms(2);
xip=.5*(-c+sqrt(c*c-4*k));
xim=.5*(-c-sqrt(c*c-4*k));
b=[1,1;xip,xim];
ac=b\y0;
yfc(:,1)=ac(1)*exp(xip*range) + ac(2)*exp(xim*range);
yfc(:,2)=ac(1)*xip*exp(xip*range) + ac(2)*xim*exp(xim*range);
yfex=real(yfc);

