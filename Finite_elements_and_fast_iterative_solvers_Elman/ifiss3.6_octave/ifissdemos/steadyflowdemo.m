%STEADYFLOWDEMO steady isothermal flow over a step 
%global pde domain
close all, batchmode('NS2'), 
figure(30), set(gcf,'Position',[0,450,450,450],'Visible','off');
figure(67), set(gcf,'Visible','off');
fprintf('\nCHECK OUT the backward-facing step steady flow solution \n'),
figure(33),  set(gcf,'Position',[0,450,450,450]); 
title('Stokes flow solution','FontSize',12),
figure(66), set(gcf,'Position',[450,450,450,450]); 
title('Navier-Stokes flow solution','FontSize',12), pause(5)
load batchrun.mat
% volume of flow
[uxref, uyref] = flowvolume(qmethod,xy,xns,0,18,'.-r',0); hold on
[uxref, uyref] = flowvolume(qmethod,xy,xns,1,18,'.-m',0); 
[uxref, uyref] = flowvolume(qmethod,xy,xns,5,18,'.-b',0);
legend('x=0','x=1','ouflow');
fprintf('\nCHECK OUT the volume of flow X-section profiles\n'),
figure(66), set(gcf,'Position',[0,450,450,450]); drawnow
figure(18),set(gcf,'Position',[450,450,450,450]);  pause(20)
set(gcf,'Position',[0,450,450,450],'Visible','off');
% vorticity
[w]= step_vorticityplot(qmethod,xns,By,Bx,G,xy,x,y,bound,1,69);
fprintf('\nCHECK OUT the computed vorticity solution ...\n'),
figure(69), set(gcf,'Position',[450,450,450,450]); drawnow,  pause(10)
[wbound] = step_bdryvorticity(domain,qmethod,xy,bound,w,19,'ob-'); 
close(19)
fprintf('\nCHECK OUT the vorticity distribution on the bottom wall\n'),
figure(69), set(gcf,'Position',[0,450,450,450]);
figure(20), set(gcf,'Position',[450,450,450,450]); pause(20)
set(gcf,'Position',[0,450,450,450],'Visible','off');
figure(69), set(gcf,'Visible','off');
figure(66), set(gcf,'Position',[0,350,350,350],'Visible','on');
%%%
fprintf('\n\nFinally, CHECK the iterative solver convergence ...\n'),
pause(5)
% iterative solvers
batchmode('itsolve_NSx1')
figure(1), set(gcf,'Position',[0,350,350,350]); drawnow
title('NEWTON system | pressure convection-diffusion','FontSize',12),
legend('ideal')
figure(1), set(gcf,'Position',[0,350,350,350]); drawnow, 
fprintf('End of steady NS flow demo. Voila!\n'),
figure(30), set(gcf,'Visible','on');
figure(33), set(gcf,'Visible','on');
figure(67), set(gcf,'Visible','on');
figure(69), set(gcf,'Visible','on');
figure(66), set(gcf,'Position',[0,450,450,450]);
figure(1), set(gcf,'Position',[450,450,450,450]);
