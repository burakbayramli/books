function bld090201
% Bubble-Funktion
clear, clc, clf
X = [0,0]; Y = [0;1]; Z = [0,0];
%plot3(X,Y,Z,'k','linewidth',1.5), hold on
X = [0,1]; Y = [0;0]; Z = [0,0];
plot3(X,Y,Z,'k','linewidth',1.5), hold on
X = [0,1]; Y = [1;0]; Z = [0,0];
plot3(X,Y,Z,'k','linewidth',1.5), hold on

NN = 30;
X = linspace(0,1,NN);
Y = linspace(0,1,NN);
Z = NaN*zeros(NN,NN);
for K = 1:NN
    for I = 1:NN-K+1
    Z(I,K) = 6*X(I)*Y(K)*(1 -X(I) - Y(K));
    end
end
%axis equal tight
mesh(X,Y,Z,'edgecolor','k'), hold on
%get(h)
axis equal tight
grid on
axis off
view(60,30)
