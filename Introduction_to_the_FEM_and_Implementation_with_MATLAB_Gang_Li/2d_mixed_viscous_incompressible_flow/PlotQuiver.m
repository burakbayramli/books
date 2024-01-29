load sol.dat; % load solution
figure(1)
quiver(sol(:,2), sol(:,3), sol(:,4), sol(:,5),2.0,...
       'Color',[0 0 0],'LineWidth',2);   % plotting
axis([0 1 0 1.1]);       % set axis range
xlabel('x-coordinate');  % set xlabel
ylabel('y-coordinate');  % set ylabel
set(gca,'fontsize',16);  % set font size