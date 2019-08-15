function bld030603b
clf
% -- B ----------------------
RBX = [-1.5,    0,  0, -1.5, - 1.5];
RBY = [-0.5, -0.5, 1,   1, -0.5];
fill(RBX,RBY,'y'), hold on
% -- A -------------------------
RAX = [1.5,    3,  3,-1.5,1.5 ];
RAY = [-0.5,-0.5,2.5, 2.5,-0.5];
fill(RAX,RAY,'y'), hold on

% -- X-Achse ----------------------
X = [-1.4,2.9]; Y = [0,0];
c = 0.15; d = 0.07;
arrow(X,Y,c,d,'k',2)
% -- Y-Achse --------------------------
X = [0,0]; Y = [-0.4,2.4];
arrow(X,Y,c,d,'k',2)
% -- Schnittpunkt + Tangente ---------------
X1 = [-1.5, 1.5];
Y1 = 1 - X1;
plot(X1,Y1,'k','linewidth',2), hold on
circle(0,1,0.05,'w')
% --Rahmen --------------------------
X5 = [-1.5,3,3,-1.5,-1.5];
Y5 = [-0.5,-0.5,2.5,2.5,-0.5];
plot(X5,Y5,'k','linewidth',2)


text(1.5,1.5,'A','fontsize',48)
text(-1,0.4,'B','fontsize',32)
text(0.1,2.3,'\rho','fontsize',24)
text(2.6,0.2,'\Gamma','fontsize',24)

axis equal
axis off
%grid on
