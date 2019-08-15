function bild01
load daten M1 M2 X Y CENTER% LU OR % demo2.m
clf
NN = 2; % Anzahl Randpunkte im farbigen Dreieck
        % von Scheibe A
%set(gca,'nextplot','replacechildren');
%axis([LU(1)-1,UO(1)+1,LU(2)-1,UO(2)+1])
axis([-7 7 -7 7]) % Bsp. 04
axis([-8 8 -8 8]) % Bsp. 10

grid on, axis equal, axis manual, hold on
X = [X, X(:,1)]; Y = [Y, Y(:,1)];
plot(X(1,:),X(2,:),'r','linewidth',2), hold on
plot(Y(1,:),Y(2,:),'b','linewidth',2), hold on
plot(CENTER(1,:),CENTER(2,:),'k--','linewidth',2), hold on
NN = 3; RR = 0.15;
AA = [M1,X(:,1:NN),M1];
circle(Y(1,1),Y(2,1),RR,'b'), hold on
circle(X(1,1),X(2,1),RR,'r'), hold on
%fill(AA(1,:),AA(2,:),'r','erasemode','none'), hold on
%plot(Y(1,:),Y(2,:),'.b'), hold on
circle(M1(1),M1(2),RR,'k')
circle(M2(1),M2(2),RR,'k')
axis off
