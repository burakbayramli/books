function bild05
% Figures for DEMO5.M
% example with exact solution after BOUKIR
% in unit square

format short, format compact
demo_number = 100;
while ~ismember(demo_number,[1,2,3,4,5,6])
   demo_number = input(' Select demo5a/b/c/d/e/f! (1/2/3/4/5/6) ');
end
%demo_number = 3;
switch demo_number
case 1, load daten5aa p e t p1 t1 FF3, load daten5ab U V P,
        FF4 = 'bsp05k'; %stat. problem
case 2, load daten5ba p e t p1 t1 FF3, load daten5bb U V P,
        FF4 = 'bsp05k'; % DAE problem 
case 3, load daten5ca p e t p1 t1 FF3, load daten5cb U V P,
        F4 = 'bsp05k';
case 4, load daten5da p e t p1 t1 FF3, load daten5db U V P,
        FF4 = 'bsp05k';
case 5, load daten5ea p e t p1 t1 FF3, load daten5eb UU0,
        FF4 = 'bsp05k';
        N1 = size(p,2); N = N1 + size(p1,2);
        U = UU0(1:N); V = UU0(N+1:2*N); P = UU0(2*N+1:end);
case 6, load daten5fa p e t p1 t1 FF3, load daten5fb U V P 
        FF4 = 'bsp05k';
end
b = 10;
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P.';
% -- exact solution ----------------
UE = b*sin(pi*X).*sin(pi*Y);
VE = b*cos(pi*X).*cos(pi*Y);
PE = 2*pi*cos(pi*X).*sin(pi*Y);

bilda = 100;
while ~ismember(bilda,[1,2,3,4,5,6])
   bilda = input(' Select figure! (1/2/3/4/5/6) ');
end
%bilda = 4;
nodes = size(p,2) + size(p1,2)
clf, hold on
frame = [0, 1, 1, 0;
        -0.5, -0.5, 0.5, 0.5]; 
frame = [frame,frame(:,1)];
plot(frame(1,:),frame(2,:),'k','linewidth',2), hold on
plot(-0.02,-0.502,'.w','markersize',6), hold on
plot(1.03,0.503,'.w','markersize',6)
axis equal tight, axis manual

N1 = size(p,2); U1 = U(1:N1).'; V1 = V(1:N1).';
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P.';
pp = [p,p1]; XX = pp(1,:); YY = pp(2,:); 
xlin    = linspace(min(X),max(X),30);
ylin    = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','g'), hold on
for I = 1:size(e,2)
   A = [p(1,e(1:2,I))]; B = [p(2,e(1:2,I))];
   %plot(A,B,'r','linewidth',2), hold on
end
%plot(p1(1,:),p1(2,:),'k*','markersize',6), hold on
% pause
axis equal, axis manual
switch bilda
case 1,
   disp(' pressure exact/numerical (red/black)')
   W = griddata(X,Y,P,X1,Y1,'cubic');
   [C,h] = contour(X1,Y1,W,[-5,-4,-3,-2,-1,0,1,2,3,4,5,],'k','linewidth',2);
 %  [C,h] = contour(X1,Y1,W,10,'k');
   clabel(C,h);
   W = griddata(X,Y,PE,X1,Y1,'cubic');
  [C,h] = contour(X1,Y1,W,[-5,-4,-3,-2,-1,0,1,2,3,4,5,],'r:','linewidth',2);
  % [C,h] = contour(X1,Y1,W,10,'r');
   clabel(C,h);
case 2, disp(' U exact/numerical (red/black)')
   W = griddata(X,Y,U1,X1,Y1,'cubic');
     [C,h] = contour(X1,Y1,W,[-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6],'k','linewidth',2);
%   [C,h] = contour(X1,Y1,W,10,'k');
   % clabel(C,h);
   W = griddata(X,Y,UE,X1,Y1,'cubic');
   [C,h] = contour(X1,Y1,W,[-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6],'r:','linewidth',2);
%   [C,h] = contour(X1,Y1,W,10,'r');
case 3, disp(' V exact/numerical (red/black)')
   W = griddata(X,Y,V1,X1,Y1,'cubic');
   [C,h] = contour(X1,Y1,W,[-5,-4,-3,-2,-1,0,1,2,3,4,5,],'k','linewidth',2);
   W = griddata(X,Y,VE,X1,Y1,'cubic');
   [C,h] = contour(X1,Y1,W,[-5,-4,-3,-2,-1,0,1,2,3,4,5,],'r:','linewidth',2);
case 4   
   NU = 1/10;  % coeff. of viscosity [m*m/sec]
   P = pressure(FF3,p,e,p1,t,t1,U,V,NU,1,FF4);
   P = P(1:N1);
   W = griddata(X,Y,P,X1,Y1,'cubic');
   [C,h] = contour(X1,Y1,W,[-5,-4,-3,-2,-1,0,1,2,3,4,5,],'k');
   %contour(X1,Y1,W,[-0.5,-8,-7],'r');
   %contour(X1,Y1,W,[0,0],'b');
  % [C,h] = contour(X1,Y1,W,10,'k','linewidth',2);
 %  clabel(C,h,'manual');
    clabel(C,h); 
   % exact contours
     W = griddata(X,Y,PE,X1,Y1,'cubic');
  [C,h] = contour(X1,Y1,W,[-5,-4,-3,-2,-1,0,1,2,3,4,5,],'r:','linewidth',2);
  % [C,h] = contour(X1,Y1,W,10,'r');
   clabel(C,h);

case 5, disp(' flow ')
   qq = quiver(X,Y,U1,V1,0,'.'); hold on
   XB = p1(1,:); YB = p1(2,:);
   UB = U(N1+1:end).'; VB = V(N1+1:end).';  
   pause
   delete(qq)
   quiver(XB,YB,UB,VB,0,'.','r'), hold on
case 6, disp(' Find index of some special points ')
   N = find_number(p,t)   
   %load daten12d Z 
   %ZWERT = Z(N)
end
axis off
clear
  