function fig0206
% Figure 2.6, Interpolating spline

clc, clf
r = 0.07; % radius of circle
B = [11; 3];
A = [4 1 ;  1 4];
D = A\B;
% ------------------
%D = 3*D;
% -------------------
D = [1; D;3];
% -------------------------------
X1 = [0 3  6  9];
%plot(X1,D,'o','Markersize',6), hold on
% -------------------------------
X2 = [0    1      2        3    4    5    6  7 8    9];
Y2 = [D(1); (2*D(1) + D(2))/3; (D(1) + 2*D(2))/3;
      D(2); (2*D(2) + D(3))/3; (D(2) + 2*D(3))/3;
      D(3); (2*D(3) + D(4))/3; (D(3) + 2*D(4))/3; D(4)];
Y2 = Y2';
plot(X2,Y2,'k','linewidth',2), hold on
plot(X2,Y2,'k','linewidth',2), hold on
% ----------------------------
B = [1 2 1 3];
X2 = [0    1      2        3    4    5    6  7 8    9].';
Y2 = [D(1); (2*D(1) + D(2))/3; (D(1) + 2*D(2))/3;
      B(2); (2*D(2) + D(3))/3; (D(2) + 2*D(3))/3;
      B(3); (2*D(3) + D(4))/3; (D(3) + 2*D(4))/3; D(4)];
plot(X2,Y2,'k','linewidth',2), hold on
% ----------------------------
%X3 = [0  3   6   9];
Y3 = [1  2   1   3];
X = linspace(0,3,60);
S = n_spline(Y3,X);
Y = 3*X(1:end-1);
plot(Y,S,'k','linewidth',2)
% ----------------------------
circle(0,1,r,'w')
circle(3,2,r,'w')
circle(6,1,r,'w')
circle(9,3,r,'w')
X1 = [0 3  6  9];
circle(X1(1),D(1),r,'w')
circle(X1(2),D(2),r,'w')
circle(X1(3),D(3),r,'w')
circle(X1(4),D(4),r,'w')
for I = 1:length(Y2)
   circle(X2(I),Y2(I),r,'w')
end   
axis([0 9 0 3]);

axis equal tight
grid off
text(0,0.5,'d_{0}=b_{0}','fontsize',18);
text(3.2,2.7,'d_{1}','fontsize',18);
text(6.2,0,'d_{2}','fontsize',18);
text(8,3.4,'d_{3}=b_{9}','fontsize',18);
text(2.9,1.6,'b_{3}','fontsize',18);
text(5.8,1.5,'b_{6}','fontsize',18);
