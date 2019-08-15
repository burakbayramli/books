function ch1ex3
% The two body problem in elliptic motion, D5 of the Hull et 
% alia test set. Solve with a crude tolerance to show the
% orbit spiraling in. y(1) is x(t), y(2) = y(t), the 
% coordinates of the moving body. y(3) = x'(t), y(4) = 
% y'(t). The eccentricity e = 0.9 for D5.
y0 = zeros(4,1);
e = 0.9;
y0(1) = 1 - e;
y0(4) = sqrt((1+e)/(1-e));
opts = odeset('RelTol',1e-6,'AbsTol',1e-10);
[tacc,yacc] = ode15s(@f,[0 20],y0,opts);
[t,y] = ode15s(@f,[0 20],y0);
%plot(yacc(:,1),yacc(:,2),y(:,1),y(:,2),0,0,'r*');
plot(yacc(:,1),yacc(:,2),'-k',y(:,1),y(:,2),'-.k',0,0,'k*')
%print -depsc ch1fig3
% Compute the energy.
r = sqrt(y(:,1).^2 + y(:,2).^2);
E = 0.5*(y(:,3).^2 + y(:,4).^2) - 1./r;
fprintf('The energy ranges from %g to %g.\n',max(E),min(E));
% Compute the angular momentum.
mv = y(:,1) .* y(:,4) - y(:,2) .* y(:,3);
fprintf('The angular momentum ranges from %g to %g.\n',max(mv),min(mv));
%===================================================
function dydt = f(t,y)
dydt = zeros(4,1);
r = sqrt(y(1)^2 + y(2)^2);
dydt(1) = y(3);
dydt(2) = y(4);
dydt(3) = - y(1)/r^3;
dydt(4) = - y(2)/r^3;