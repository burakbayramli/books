function getch1fig8
% Contrast local and global error
% Initialize output arrays.
t = [];
le = [];
ge = [];
% Initialize for integration.
tn = 0;
yn = 2;
h = 0.1;
for i = 1:30
    tnp1 = tn + h;
    ynp1 = yn + h*ode(tn,yn);
    len = localsol(tn,yn,tn+h) - ynp1;
    gen = globalsol(tn+h) - ynp1;
    t = [t tnp1];
    le = [le len];
    ge = [ge gen];
    tn = tnp1;
    yn = ynp1;
end
%plot(t,le,'o',t,ge,'*')
plot(t,le,'ok',t,ge,'*k','MarkerSize',3)
legend('local error','global error',3)
axis([0 3 -0.2 0.1])
%print -depsc ch1fig8
%===============================================
function dydt = ode(t,y)
dydt = cos(t)*y;

function gy = globalsol(t)
% Global solution.
gy = 2*exp(sin(t));

function ly = localsol(tn,yn,t)
% Local solution.
ly = yn*exp((sin(t) - sin(tn)));