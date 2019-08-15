function bld070102
% Balkenbiegung, Skizze 2
clf
c = 0.2;
d = 0.07;
X = [-0.5,5];
Y = [0, 0];
arrow(X,Y,c,d,'k',2)    % x-Achse.
hold on
c = 0.2;
d = 0.07;
X = [0, 0];
Y = [1,-1];
arrow(X,Y,c,d,'k',2)    % y-Achse
hold on
X = linspace(-0.4,4.4,30);
Y = 0.15*X.*(X - 4);
plot(X,Y,'linewidth',2)
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%
c = 0.2;
d = 0.07;
X = [3,3];
Y = [0.5,-0.3];
arrow(X,Y,c,d,'k',2)    % f(x)
hold on
c = 0.2;
d = 0.07;
X = [1.4,1.4];
Y = [-1,-0.2];
arrow(X,Y,c,d,'k',2)    % q(x)
hold on
c = 0.2;
d = 0.07;
X = [-0.5,0];
Y = [0.3,0.3];
arrow(X,Y,c,d,'k',2)    % P links
hold on
c = 0.2;
d = 0.07;
X = [4.5,4];
Y = [0.3,0.3];
arrow(X,Y,c,d,'k',2)    % P rechts
hold on
X = [4,4];
Y = [-0.5,0.5];
plot(X,Y,'--')    % Strich rechts
hold on
text(4.6,-0.2,'x','Fontsize',28)
text(0.1,-0.8,'y','Fontsize',28)
text(3.2,0.4,'f(x)','Fontsize',22)
text(1.5,-1,'r(x)u(x), r(x) > 0','Fontsize',22)
text(-0.3,0.6,'q','Fontsize',22)
text(4.1,0.6,'q','Fontsize',22)
axis equal tight
%title('Skizze 1','Fontsize',18)
axis equal tight
axis off
