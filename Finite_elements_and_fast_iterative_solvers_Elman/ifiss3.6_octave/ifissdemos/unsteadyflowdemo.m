%UNSTEADYFLOWDEMO unsteady isothermal flow in a lid-driven cavity
ifiss
close all 
fprintf('\nDriven cavity unsteady flow  ... \n'),
fprintf('running STABTR to 100 time units ... \n'), pause(2)
batchmode('T-NS3'), load unsteadyrun.mat
figure(14), drawnow
fprintf('\nCHECK OUT the time step history \n'), pause(10),
figure(14), set(gcf,'Visible','off');
figure(100), drawnow
fprintf('\nCHECK OUT the solution evolution \n'), pause(8),
figure(100), set(gcf,'Visible','off');
if vswitch==1, figure(167), set(gcf,'Visible','off'); end
snaptime=[50,80,length(time)];
if qmethod==1
square_unsteadyflowref(1,ev,U,time,A,By,Bx,G,xy,xyp,x,y,bound,snaptime);
elseif qmethod>1
square_unsteadyflowref(2,mv,U,time,A,By,Bx,G,xy,xyp,x,y,bound,snaptime);
end
fprintf('\nCHECK OUT the snapshots of the flow evolution \n'), 
figure(101), set(gcf,'Position',[0,0,350,750],'Visible','on'); drawnow, pause(10)
flowxsection(domain,qmethod,xy,U(:,end),0,12,'.b');
fprintf('\nCHECK OUT the final time solution mid-plane X-section \n'), 
figure(12), set(gcf,'Position',[350,350,550,450],'Visible','on'); drawnow, pause(10)
figure(12), set(gcf,'Visible','off');
figure(101), set(gcf,'Position',[0,0,350,750],'Visible','on');
figure(14), set(gcf,'Position',[900,400,350,350],'Visible','on');
%%%
fprintf('\n\nCHECK the iterative solver convergence ...\n'),
pause(2)
% iterative solvers
batchmode('snapshot_flowx1')
figure(1), set(gcf,'Position',[900,400,350,350]); drawnow,
batchmode('snapshot_flowx2')
title('PCD* inexact preconditioning','FontSize',12),
legend('AMG-ILU', 'AMG-PDJ')
figure(1), set(gcf,'Position',[900,400,350,350]); drawnow,
fprintf('End of unsteady NS flow demo. Voila!\n'),
