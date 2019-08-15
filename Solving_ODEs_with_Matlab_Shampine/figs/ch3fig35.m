function sol = ch3ex3
solinit = bvpinit(linspace(0,1,5),@guess,2*pi);
sol = bvp4c(@ode,@bc,solinit);
T = sol.parameters;
fprintf('The computed period T = %g.\n',T);
%plot(T*sol.x,sol.y(1,:),T*solinit.x,solinit.y(1,:),'ro')
plot(T*sol.x,sol.y(1,:),'-k',T*solinit.x,solinit.y(1,:),'ko')
legend('Computed Solution','Guessed Solution');
axis([0 T -2.2 2]);
%print -depsc ch3fig3
%=================================================
function v = guess(x)
v = [ sin(2*pi*x); cos(2*pi*x)];

function dydt = ode(x,y,T);
dydt = [ 3*T*(y(1) + y(2) - (y(1)^3)/3 - 1.3)
         -(T/3)*(y(1) - 0.7 + 0.8*y(2))       ];

function res = bc(ya,yb,T)
res = [ ya(1); yb(1); (ya(2) - yb(2)) ];