function bld090101
% Haengender Knoten
clf
KNOTEN = [0, 1, 1, 0, 0.5;
          0, 0, 1, 1, 0.5];
RAND = [KNOTEN(:,1:4), KNOTEN(:,1)];
plot(RAND(1,:),RAND(2,:), 'k','linewidth',2), hold on
axis([-0.2 1.2 -0.2 1.2]), axis equal
X = [0,1];   Y = [0,1];   plot(X,Y,'k','linewidth',2), hold on
X = [0.5,1]; Y = [0.5,0]; plot(X,Y,'k','linewidth',2), hold on
RR = 0.02;
circle(0,0,RR,'w')
circle(0,1,RR,'w')
circle(1,0,RR,'w')
circle(1,1,RR,'w')
circle(0.5,0.5,0.03,'y')
axis off
