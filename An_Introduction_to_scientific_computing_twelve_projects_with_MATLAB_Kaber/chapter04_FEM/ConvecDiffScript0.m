%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% on l'on s'amuse avec la solution exacte 
f = inline('ones(size(x))');          %second membre de l'equation
x=linspace(0,1,100);                  %grille de calcul
u1_1=ConvecDiffSolExa(1,1,1,x);%solution exacte calculee sur x
u05_1=ConvecDiffSolExa(.5,1,1,x);%solution exacte calculee sur x
u01_1=ConvecDiffSolExa(.1,1,1,x);%solution exacte calculee sur x
u001_1=ConvecDiffSolExa(.01,1,1,x);%solution exacte calculee sur x
u1_m1=ConvecDiffSolExa(1,-1,1,x);%solution exacte calculee sur x
u05_m1=ConvecDiffSolExa(.5,-1,1,x);%solution exacte calculee sur x
u01_m1=ConvecDiffSolExa(.1,-1,1,x);%solution exacte calculee sur x
u001_m1=ConvecDiffSolExa(.01,-1,1,x);%solution exacte calculee sur x
%
set(gca,'XTick',0:.2:1,'FontSize',20)
set(gca,'YTick',0:.2:1)
figure(1);plot(x,u1_1,x,u05_1,x,u01_1,x,u001_1,'LineWidth',4)
title('Solution du problème de convection diffusion, \lambda =1');
text(.75,.9,'\fontsize{20}\epsilon=0.01');text(.7,.6,'\fontsize{20}\epsilon=0.5');
text(.6,.3,'\fontsize{20}\epsilon=0.1');text(.5,.05,'\fontsize{20}\epsilon=1');
%
pause
set(gca,'XTick',0:.2:1,'FontSize',20)
set(gca,'YTick',0:.2:1)
figure(2);plot(x,u1_m1,x,u05_m1,x,u01_m1,x,u001_m1,'LineWidth',4)
title('Solution du problème de convection diffusion, \lambda =-1');
text(.2,.9,'\fontsize{20}\epsilon=0.01');text(.2,.6,'\fontsize{20}\epsilon=0.5');
text(.3,.3,'\fontsize{20}\epsilon=0.1');text(.5,.05,'\fontsize{20}\epsilon=1');
