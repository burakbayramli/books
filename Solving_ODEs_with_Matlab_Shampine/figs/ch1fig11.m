% Example of bifurcation
t = linspace(0,1);
y1 = sqrt(t);
y2 = -sqrt(t);
set(gcf,'DefaultAxesColorOrder',[0 0 0; 0 0 0; 0 0 0])
%plot([-0.1 1.1],[0 0],'k',[0 0],[-1.1 1.1],'k',...
%         0,0,'ro',1,1,'r*',t,y1,t,y2);
plot([-0.1 1.1],[0 0],'k',[0 0],[-1.1 1.1],'k',...
	 0,0,'ko',1,1,'k*',t,y1,'-k',t,y2,'-k',...
     'LineWidth',1,'MarkerSize',5);
axis([-0.1 1.1 -1.1 1.1])
%print -depsc ch1fig5