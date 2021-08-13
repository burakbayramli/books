%STOKESFLOWDEMO EST_MINRES demonstration
ifiss
close all, batchmode('S3'), 
fprintf('\nCHECK OUT the Stokes driven cavity flow solution\n'),
%%% positioning is [left,bottom,width,height]
figure(30), set(gcf,'Position',[0,450,450,450],'Visible','off');
figure(33), set(gcf,'Position',[0,450,450,450]); 
figure(34), set(gcf,'Position',[450,450,450,450]); pause(15) 
figure(34), set(gcf,'Position',[0,450,450,450],'Visible','off');
figure(33), set(gcf,'Position',[0,350,350,350],'Visible','on'); drawnow
fprintf('\n\nCHECK the iterative solver convergence ...\n'),
batchmode('itsolve_stokesx1'), load batchrun_itsolve.mat
[np,nu]=size(Bst);
xdiff=norm(xest(1:nu)-xst(1:nu),inf);
fprintf('velocity solution difference is %7.3e\n',xdiff),
figure(1),title('ESTMINRES | convergence comparison','FontSize',12),
figure(1), set(gcf,'Position',[0,450,450,450]);
fprintf('\nEnd of Stokes flow demo. Voila!\n'),
