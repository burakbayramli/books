
% Run xclaw in qref directory to make xref,qref (exact solution)
% Run xclaw in this directory with different settings in claw2ez.data to make
%   qss1:  method(2)=1 and method(5)=2  (upwind with strang splitting)
%   qgs2:  method(2)=2 and method(5)=1  (Lax-Wendroff with Godunov splitting)
%   qss2:  method(2)=2 and method(5)=2  (Lax-Wendroff with strang splitting)

hp =plot(x,qss1,'.');
set(hp,'MarkerSize',20)
hold on
axis([.5 1 -.1 1.2])
plot(x,qgs2,'o','MarkerSize',8)
plot(x,qss2,'+','MarkerSize',8)


hl = legend('Upwind with Strang splitting',...
       'Lax-Wendroff with Godunov splitting',...
       'Lax-Wendroff with Strang splitting');

plot(xref,qref)
hold off

% print nocommuteq -deps
