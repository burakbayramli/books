%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise 5.1
%% Graphical display of the  linear combination L_0-2L_1+3L_5 on
%% interval [-1,1]. 
%% Comparison of computing time with the matlab function legendre 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close all
x=linspace(-1,1,5000);  % points for graphical display
deg=5;
c=[1;-2;0;0;0;3]
%deg=50;            
%c=rand(deg+1,1);
tic;
y=SPE_LegLinComb(x,c);
Rec=toc;
tic;
L=legendre(0,x);
z=c(1)*L(1,:);
for i=1:deg
  L=legendre(i,x);
  z=z+c(i+1)*L(1,:);
end
Mat=toc;
plot(x,y,x,z)
legend('recurrence','matlab')
title(['Lin. Comb of Legendre pol. of degree ',num2str(deg)])
fprintf(' CPU for recurrence= %f\n CPU for  matlab function= %f\n',Rec,Mat);
