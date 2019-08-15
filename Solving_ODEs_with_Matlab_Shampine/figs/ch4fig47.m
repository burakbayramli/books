function sol = ex10ch4
history = [80; 30];
tspan = [0, 100];
opts = ddeset('RelTol',1e-5,'AbsTol',1e-8);
% Solve the ODEs that arise when there is no delay.
sol0 = dde23(@ddes,[],history,tspan,opts);
% Solve the DDEs that arise when there is a delay of tau.
tau = 1;
sol1 = dde23(@ddes,tau,history,tspan,opts);
%plot(sol0.y(1,:),sol0.y(2,:),sol1.y(1,:),sol1.y(2,:))
plot(sol0.y(1,:),sol0.y(2,:),'-k',...
     sol1.y(1,:),sol1.y(2,:),'--k')
%title('Predator--Prey Solution With and Without Delay')
title('')
xlabel('y_1(t)')
ylabel('y_2(t)')
legend('No delay',['Delay \tau = ',num2str(tau)],2)
%=======================================================
function v = ddes(t,y,Z)
a =  0.25; b = -0.01; c = -1.00; d =  0.01;
v = zeros(2,1);
if isempty(Z)     % ODEs
   v(1) = a * y(1) + b * y(1) * y(2);
   v(2) = c * y(2) + d * y(1) * y(2);
else              % DDEs
   m = 200;
   ylag = Z(:,1);
   v(1) = a * y(1) * (1 - y(1) / m) + b * y(1)    * y(2);
   v(2) = c * y(2)                  + d * ylag(1) * ylag(2);
end