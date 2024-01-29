function perform_postprocessing(order)
  
load T.dat;
load nodes.dat;
n_nodes=size(T,1);

%%--------------------------------------------------------
%% 3D plot if you like to see
%%--------------------------------------------------------
 figure(1)
 clf
% Plot the nodal temperatures as points
%plot3(nodes(:,2),nodes(:,3),T(:,2),'o')
scatter3(nodes(:,2),nodes(:,3),T(:,2),40,T(:,2),'s','filled')
hold on;
scatter3(-nodes(:,2),nodes(:,3),T(:,2),40,T(:,2),'s','filled')
scatter3(-nodes(:,2),-nodes(:,3),T(:,2),40,T(:,2),'s','filled')
scatter3(nodes(:,2),-nodes(:,3),T(:,2),40,T(:,2),'s','filled')
view(0,90);
axis([-0.3 0.3 -0.3 0.3]);
print -depsc -r150 a.eps
print -dpng -r150 a.png

%
% Plot a surface of the exact solution
% hold on
% [xmes,ymes]=meshgrid(0:.1:1, 0:.1:1);
% Texact = -xmes .^3 - ymes .^3 + 3 .* xmes .* ymes .^2 + 3 .* xmes.^2 .* ymes;
% surf(xmes,ymes,Texact)
% xlabel('x')
% ylabel('y')
% zlabel('T')
% hold off
% campos([-5.4508    6.7709    4.0684])
%%--------------------------------------------------------

%figure(order)
%clf
%x=[0:0.01:1];
%plot(x, -x.^3 -0.25^3 + 3*x.^2*0.25 + 3*x*0.25^2);
%hold on
%for i=1:n_nodes
%  if nodes(i,3)==0.25
%    plot(nodes(i,2),T(i,1),'o');
%  end
%end
%xlabel('x coordinate');
%ylabel('T');
%legend('Exact solution','FE solution');

hold off

