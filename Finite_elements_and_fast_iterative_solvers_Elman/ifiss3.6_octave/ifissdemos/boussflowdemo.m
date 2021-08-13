%%ifissdemo  Boussinesq unsteady flow over a heated step 
global Ra Pr H L
close all
fprintf('\nBoussinesq flow over a heated step  ... \n'),
fprintf('running STABTR for 200 timesteps  ... \n'), pause(5)
batchmode('B-NS2'), load unsteadyrun.mat
figure(102), set(gcf,'Position',[0,450,400,150]);
figure(101), set(gcf,'Position',[0,450,400,150]);
figure(105), set(gcf,'Position',[0,700,400,150]);
figure(13), set(gcf,'Position',[900,450,350,350],'Visible','on');
fprintf('\nCHECK OUT the time step history and final solution \n'), pause(10)
figure(13), set(gcf,'Visible','off');
ppbouss_checkpointdata
close(200)
save unsteadyrun.mat
fprintf('\nFinally, CHECK the iterative solver convergence ...\n'),
pause(5)
% iterative solvers
batchmode('snapshot_boussx1')
figure(19), set(gcf,'Position',[900,400,350,350]); drawnow, 
batchmode('snapshot_boussx2')
title('inexact preconditioning','FontSize',12),
legend( 'AMG-PCD*','AMG-LSC')
figure(19), set(gcf,'Position',[900,400,350,350]); drawnow, 
fprintf('End of Boussinesq flow demo. Voila!\n'),
