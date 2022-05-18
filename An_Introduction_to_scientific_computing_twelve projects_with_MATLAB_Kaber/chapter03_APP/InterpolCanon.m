%scripts du projet approximation INTERPOLATION

function InterpolCanon() %Runge 
n=30;
x=-1+2*(0:n)'/n;
g=(-1:.02:1)';
y=test1Interpol(x,g);
plot(g,f(g),g,y,'r+','LineWidth',2,'MarkerSize',10)
set(gca,'XTick',-1:.4:1,'FontSize',24)
%set(gca,'YTick',-1:.2:1)
legend('u','I_n u')
titre=['Phénomène de Runge, n = ' num2str(n)];
title(titre);

function y=f(x)
a=.4;y=1./(x.*x+a*a);

function y=test1Interpol(x,G)
%calcul de l'interpolee dans la base 1
%sur la grille G
%interpolation aux points de x (vecteur colonne)
%On calcule la matrice A
A=ones(length(x),1);
for k=1:length(x)-1
    A=[A x.^k];
end;
%c=cond(A);
cf=A\f(x);
%remettre dans le bon ordre les coefficients
cf=cf(end:-1:1);
y=polyval(cf,G);%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%y=polyval(cf,x);

