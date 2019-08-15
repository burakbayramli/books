function ch2ex2
opts = odeset('Events',@events,'RelTol',1e-6,'AbsTol',1e-10);
[t,x,te,xe,ie] = ode45(@odes,[0 65],[5; 5; 5; 5],opts);
plot3(x(:,1),x(:,2),x(:,3));
xlabel('x_1(t)'), ylabel('x_2(t)'), zlabel('x_3(t)')
if isempty(ie)
    fprintf('There were no events.\n');
else
    event1 = find(ie == 1);
    if isempty(event1)
        fprintf('Event 1 did not occur.\n');
    else
        fprintf('Event 1 occurred %i times.\n',length(event1));
        figure
        plot(xe(event1,1),xe(event1,3),'*');
    end
    event2 = find(ie == 2);
    if isempty(event2)
        fprintf('Event 2 did not occur.\n');
    else
        fprintf('Event 2 occurred %i times.\n',length(event2));
        figure
        plot(xe(event2,1),xe(event2,2),'*');
    end
end
%==============================================================
function dxdt = odes(t,x)
a = 3.12121212;
b = 2.11111111;
dxdt = [a*x(3); b*x(4); -a*x(1); -b*x(2)];

function [value,isterminal,direction] = events(t,x)
value = [x(2); x(3)];
isterminal = [0; 0];
direction = [0; 0];