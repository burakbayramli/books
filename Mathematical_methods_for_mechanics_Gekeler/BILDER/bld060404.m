function bld110501
% Doppelpendel
clf, clear, clc
plot(0.4,-2.2,'w.'), hold on
plot(2.1,0.1,'w.'), hold on
axis equal tight, axis manual
c = 0.09; d = 0.03;
X = [0.4,1.5]; Y = [0,0];
arrow(X,Y,c,d,'k',2)
X = [0.5,0.5]; Y = [0.1,-1];
arrow(X,Y,c,d,'k',2)
X = [1,1.5]; Y = [-0.2,-1];
plot(X,Y,'k','linewidth',2), hold on
X = [1.5,0.8]; Y = [-1,-1.7];
plot(X,Y,'k','linewidth',2), hold on
X = [1,1]; Y = [0,-0.9];
plot(X,Y,'k--'), hold on
X = [1.5,1.5]; Y = [-0.8,-1.6];
plot(X,Y,'k--'), hold on

A = [1,-0.2]; B =[1,-0.8]; PHI = 0.58;
[X,Y] = segm(A,B,PHI,0);
plot(X,Y,'linewidth',2), hold on
A = [1.5,-1]; B = [1.5,-1.45]; PHI = -0.8;
[X,Y] = segm(A,B,PHI,0);
plot(X,Y,'linewidth',2), hold on


rr = 0.025;
circle(1,-0.2,rr,'w')
circle(1.5,-1,rr,'w')
circle(0.8,-1.7,rr,'w')
text(1.55,0,'x','fontsize',18)
text(0.45,-1.1,'y','fontsize',18)
text(1.3,-0.6,'l_1','fontsize',18)
text(1,-1.2,'l_2','fontsize',18)
text(1.05,-0.6,'\phi_1','fontsize',18)
text(1.3,-1.3,'\phi_2','fontsize',18)

text(1.6,-1.02,'(x_1,y_1)','fontsize',18)
text(0.6,-1.85,'(x_2,y_2)','fontsize',18)

grid off
axis off
