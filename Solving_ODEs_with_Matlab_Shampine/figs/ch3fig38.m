function ch3ex6
global R
%color = [ 'k', 'r', 'b' ];
color = [ 'k', 'k', 'k' ];
options = bvpset('FJacobian',@Jac,'BCJacobian',@BCJac,...
                 'Vectorized','on');
R = 100;
sol = bvpinit(linspace(0,1,10),ones(7,1),1);             
hold on
for i = 1:3
    sol = bvp4c(@ode,@bc,sol,options);
    fprintf('For R = %5i, A = %4.2f.\n',R,sol.parameters);
%   plot(sol.x,sol.y(2,:),color(i));
    if i == 1
       plot(sol.x,sol.y(2,:),'-k');
    end
    if i == 2
       plot(sol.x,sol.y(2,:),':ko','MarkerSize',2);
    end
    if i == 3 
       plot(sol.x,sol.y(2,:),'--k');
    end
    axis([-0.1 1.1 0 1.7]);
    drawnow
    R = 10*R;
end
legend('R =    100','R =   1000','R = 10000',1); 
hold off
%print -depsc ch3fig6
%================================================================
function dydx = ode(x,y,A);
global R
P = 0.7*R;
dydx = [ y(2,:); y(3,:); R*(y(2,:).^2- y(1,:).*y(3,:)-A);...
         y(5,:); -R*y(1,:).*y(5,:)-1; y(7,:); -P*y(1,:).*y(7,:) ];

function [dFdy,dFdA] = Jac(x,y,A)
global R
dFdy = [      0,       1,       0,     0,   0,     0,      0
              0,       0,       1,     0,   0,     0,      0
           -R*y(3), 2*R*y(2), -R*y(1), 0,   0,     0,      0
              0,       0,       0,     0,   1,     0,      0
           -R*y(5),    0,       0,     0, -R*y(1), 0,      0
              0,       0,       0,     0,   0,     0,      1
         -7/10*R*y(7), 0,       0,     0,   0,     0, -7/10*R*y(1) ];

dFdA = [ 0; 0; -R; 0; 0; 0; 0];

function res = bc(ya,yb,A)
res = [ ya(1); ya(2); yb(1)-1; yb(2);...
        ya(4); yb(4); ya(6); yb(6)-1 ];

function [dBCdya,dBCdyb,dBCdA] = BCJac(ya,yb,A)
dBCdya = [ 1, 0, 0, 0, 0, 0, 0
           0, 1, 0, 0, 0, 0, 0
           0, 0, 0, 0, 0, 0, 0
           0, 0, 0, 0, 0, 0, 0
           0, 0, 0, 1, 0, 0, 0
           0, 0, 0, 0, 0, 0, 0
           0, 0, 0, 0, 0, 1, 0
           0, 0, 0, 0, 0, 0, 0 ];

dBCdyb = [ 0, 0, 0, 0, 0, 0, 0
           0, 0, 0, 0, 0, 0, 0
           1, 0, 0, 0, 0, 0, 0
           0, 1, 0, 0, 0, 0, 0
           0, 0, 0, 0, 0, 0, 0
           0, 0, 0, 1, 0, 0, 0
           0, 0, 0, 0, 0, 0, 0
           0, 0, 0, 0, 0, 1, 0 ];

dBCdA = zeros(8,1);