function sol = ch3ex7
global delta
sol = bvpinit(linspace(0,11.3,5),ones(5,1));
for delta = [0 0.1 0.5 1]
    sol = bvp4c(@odes,@bcs,sol);
%    plot(sol.x,sol.y)
%    drawnow
%    pause
end
%plot(sol.x,sol.y)
plot(sol.x,sol.y(1,:),'-k',...
     sol.x,sol.y(2,:),':ko',...
     sol.x,sol.y(3,:),'--k',...
     sol.x,sol.y(4,:),'-.k',...
     sol.x,sol.y(5,:),'-k','MarkerSize',2)
axis([0 11.3 -2 1.5])
fprintf('Reference values: y_3(0) = -0.96631, y_5(0) =  0.65291\n')
fprintf('Computed values:  y_3(0) = %8.5f, y_5(0) = %8.5f\n',...
         sol.y(3,1),sol.y(5,1))
%print -depsc ch3fig7     
%=================================================
function dydt = odes(t,y)
global delta
n = -0.1;
s = 0.2;
c = -(3 - n)/2;
linear = [ y(2); y(3); 1+s*y(2); y(5); s*(y(4) - 1) ];
nonlinear = [ 0; 0; (c*y(1)*y(3) - n*y(2)^2 - y(4)^2); ...
              0; (c*y(1)*y(5) - (n-1)*y(2)*y(4))         ];
dydt = linear + delta*nonlinear;          

function res = bcs(ya,yb)
res = [ ya(1); ya(2); ya(4); yb(2); yb(4)-1 ];