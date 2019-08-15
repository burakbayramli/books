function bld090102
% Einheitsquadrat mit Dreieckszerlegung
clf, hold on
KNOTEN = [0, 1, 1, 0, 0.5;
          0, 0, 1, 1, 0.5];
ELEMENTE = [1 2 5; 2 3 5; 3 4 5; 4 1 5];
RAND = [KNOTEN(:,1:4), KNOTEN(:,1)];
plot(RAND(1,:),RAND(2,:),'k','linewidth',2), hold on
X = [0,1]; Y = [0,1];
plot(X,Y,'k','linewidth',2), hold on
X = [0,1]; Y = [1,0];
plot(X,Y,'k','linewidth',2), hold on
axis([-0.2 1.2 -0.2 1.2]), axis equal
RR = 0.02;
circle(0,0,RR,'w')
circle(0,1,RR,'w')
circle(1,0,RR,'w')
circle(1,1,RR,'w')
circle(0.5,0.5,RR,'w')
text(-0.1,-0.1,'1','fontsize',24);
text(1.1,-0.1,'2','fontsize',24);
text(1.1,1.1,'3','fontsize',24);
text(-0.1,1.1,'4','fontsize',24);
text(0.47,0.4,'5','fontsize',24);
text(0.47,0.2,'A','fontsize',24);
text(0.7,0.5,'B','fontsize',24);
text(0.47,0.8,'C','fontsize',24);
text(0.2,0.5,'D','fontsize',24);
axis off
%grid on
