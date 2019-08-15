function fig0205
% Figure 2.5, Bezier curve for B.-polynomial of degree 3

clc, clf
r = 0.07; % Radius of circle
X1 = [0 3 6 9]; Y0 = [0 0 0 0];
plot(X1,Y0,'ko','Markersize',6), hold on
X1 = [1 2 4 5 7 8]; Y0 = [0 0 0 0 0 0];
plot(X1,Y0,'k*','Markersize',6), hold on
% -------------------------------
%X1 = [0 3  6  9]; Y1 = [1 2  1  3];
%plot(X1,Y1,'o','Markersize',6), hold on
% -------------------------------
X = linspace(0,1,20);
B1 = [1 0.5 1 2];
P = decastel(B1,X);
X1= 3*X;
plot(X1,P,'k','linewidth',2), hold on
% ----------------------------
B2 = [2 2.5 0.5 1];
P = decastel(B2,X);
X2 = 3*X + 3;
plot(X2,P,'k','linewidth',2), hold on
% ----------------------------
B3 = [1 1 1.5 3];
P = decastel(B3,X);
X3 = 3*X + 6;
plot(X3,P,'k','linewidth',2), hold on
% ----------------------------
X2 = [0  3   6   9]; Y2 = [1  2   1   3];
X = linspace(0,3,60);
S = n_spline(Y2,X);
Y = 3*X; Y = Y(1:end-1);
plot(Y,S,'k--','linewidth',2)
% ----------------------------
grid off
axis([0 9 0 3]);
axis equal tight
X2 = [0 1   2  3  4    5    6  7 8    9];
Y2 = [1 0.5 1  2  2.5    0.5  1  1 1.5  3];
plot(X2,Y2,'k','linewidth',2), hold on
plot(X2,Y2,'*','Markersize',3),hold on
for I = 1:length(X2)
    circle(X2(I),Y2(I),r,'w')
end    


text(0.1,0.5,'b_{0}','fontsize',18);
text(2.9,2.5,'b_{3}','fontsize',18);
text(5.8,1.5,'b_{6}','fontsize',18);
text(9.2,3,'b_{9}','fontsize',18);

