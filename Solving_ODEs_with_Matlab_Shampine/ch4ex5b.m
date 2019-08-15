function ch4ex5b
options = optimset('Display','iter','TolX',0.01);
Acr = fzero(@f,[0.4, 1.4],options);
fprintf('\nThe critical excitation amplitude is %4.2f.\n',Acr);
%================================================
function fval = f(A)
fval = +1;
omega = 2;
tfinal = 40*pi/omega;
state = +1;
opts = ddeset('Events',@events);
sol = dde23(@ddes,0.1,[0; 0],[0 tfinal],opts,state,A);
while sol.x(end) < tfinal
   if sol.ie(end) == 1
      state = - state;
      opts = ddeset(opts,'InitialY',[ 0; 0.913*sol.y(2,end)]);
      sol = dde23(@ddes,0.1,sol,[sol.x(end) tfinal],opts,state,A);
   else
      fval = -1;
      break;
   end
end

function dydt = ddes(t,y,Z,state,A)
omega = 2; gamma = 0.248; beta  = 1; 
ylag = Z(1,1);
dydt = [y(2); 0];
dydt(2) = sin(y(1)) - state*gamma*cos(y(1)) - beta*ylag ...
          + A*sin(omega*t + asin(gamma/A));

function [value,isterminal,direction] = events(t,y,Z,state,A)
value = [y(1); abs(y(1))-pi/2];
isterminal = [1; 1];
direction = [-state; 0];