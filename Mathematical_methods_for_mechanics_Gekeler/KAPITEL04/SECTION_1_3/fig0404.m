function fig0404
% Figure 4.4, Fountain with envelope

clf, clc
c = 0.008; d = 0.003; X = [-0.112,0.112]; Y = [0, 0];
arrow(X,Y,c,d,'k',2), hold on    % x-Achse.
X = [0, 0]; Y = [0,0.065];
arrow(X,Y,c,d,'k',2), hold on    % y-Achse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
g = 10; v = 1;
X = linspace(0,0.1,300);
for K = 1:15
phi = K*pi/30;
Y = - 0.5*g*X.*X/(v*cos(phi))^2 + tan(phi)*X;
J = find(Y >= 0);
plot(X(J),Y(J),'b','linewidth',2), hold on
plot(-X(J),Y(J),'b','linewidth',2), hold on

end
Y = -0.5*g*X.*X - v^4/(2*g + g*v^2) + 0.0833;
plot(X,Y,'r','linewidth',2), hold on
plot(-X,Y,'r','linewidth',2), hold on

circle(0,v*v/(2*g),0.002,'w')
%hold on
%text(4.6,-0.2,'x','Fontsize',28)
%text(0.1,-0.8,'y','Fontsize',28)
grid on
axis equal tight
axis off
%grid off
