function sol = ch4ex5a
state = +1;
opts = ddeset('RelTol',1e-5,'Events',@events);
sol = dde23(@ddes,0.1,[0; 0],[0 12],opts,state);
ref = [4.516757065, 9.751053145, 11.670393497];
fprintf('Kind of Event:                 dde23   reference\n');
event = 0;
while sol.x(end) < 12
    event = event + 1;
    if sol.ie(end) == 1
        fprintf('A wheel hit the ground.   %10.4f  %10.6f\n',...
                sol.x(end),ref(event));
        state = - state;
        opts = ddeset(opts,'InitialY',[ 0; 0.913*sol.y(2,end)]);
        sol = dde23(@ddes,0.1,sol,[sol.x(end) 12],opts,state);
    else
        fprintf('The suitcase fell over.   %10.4f  %10.6f\n',...
                sol.x(end),ref(event));
        break;
    end
end
plot(sol.y(1,:),sol.y(2,:))
xlabel('\theta(t)')
ylabel('\theta''(t)')
%=================================================================
function dydt = ddes(t,y,Z,state)
gamma = 0.248;
beta  = 1;
A = 0.75;
omega = 1.37;
ylag = Z(1,1);
dydt = [y(2); 0];
dydt(2) = sin(y(1)) - state*gamma*cos(y(1)) - beta*ylag ...
          + A*sin(omega*t + asin(gamma/A));

function [value,isterminal,direction] = events(t,y,Z,state)
value = [y(1); abs(y(1))-pi/2];
isterminal = [1; 1];
direction = [-state; 0];