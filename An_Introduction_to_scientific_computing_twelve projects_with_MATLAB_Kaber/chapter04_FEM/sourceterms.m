%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
n=100;lambda=1;eps=0.01;
x=(0:(n+1))'/(n+1);
A=FEM_ConvecDiffAP1(eps,lambda,n);
X=[];Y=[];
h=1/(n+1);tab=(1:n)'*h;
% for af=1:5
for af=1.5:1.5
   b=h*cos(af*pi*tab);
   y=A\b;y=[0; y; 0];
   X=[X x];Y=[Y y];
end;
figure(1);
set(gca,'XTick',0:.2:1,'FontSize',20)
set(gca,'YTick',0:.2:1)
plot(X,Y,'MarkerSize',10,'LineWidth',2);
title('\fontsize{16}Solution du problème de convection diffusion');
% text(.45,.35,'\fontsize{20}a=1');
% text(.3,.15,'\fontsize{20}a=2');
% text(.45,-.15,'\fontsize{20}a=3');
% text(.6,.1,'\fontsize{20}a=4');
% text(.1,-.05,'\fontsize{20}a=5');
