function bild06
% Figures for DEMO6.M
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),
% in unit square
demo_number = 100;
while ~ismember(demo_number,[1,4,6,7,8])
   demo_number = input(' Select demo_number! (1/4/6/7/8) ');
end
%demo_number = 6;
switch demo_number
case 1, load daten1a p e t p1 t1, load daten1b U V P, flag = 1; 
case 4, load daten4a p e t p1 t1, load daten4b U V P, flag = 1; 
case 6, load daten6a p e t p1 t1, load daten6b U V P, flag = 1; 
case 7, load daten7a p e t p1 t1, load daten7b U V P, flag = 2; 
case 8, load daten8a p e t p1 t1, load daten8b U V P 
end
FF3 = 'bsp01h';
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Select figure! (1/2/3/4) ');
end

%bilda = 4;
clf, hold on
frame = [0, 1, 1, 0;
        0,  0, 1, 1]; 
frame = [frame,frame(:,1)];
plot(frame(1,:),frame(2,:),'k','linewidth',2), hold on
plot(-0.02,-0.02,'.w','markersize',6), hold on
plot(1.03,1.03,'.w','markersize',6)

axis equal tight, axis manual
N1 = size(p,2); U1 = U(1:N1).'; V1 = V(1:N1).';
nodes = N1 + size(p1,2)
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
switch bilda  
case 1, disp(' Contour of stream function Z*100 ')
   [RDU,RDV,RDP,FU,FV,RDZ,RCZ,RCP] = feval(FF3,p,e,p1);
%   LASTEN  =  rside_post(p,t,t1,U,V,RCP); % also RCP = []
    LASTEN  =  rside_post(p,t,t1,U,V,[]); % also RCP = []
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
   Z = 100*Z; %!!!!!!!!!!!!!!!!!!!
   X1 = [p(1,:),p1(1,:)];  Y1 = [p(2,:),p1(2,:)];
   %trimesh(t(1:3,:)',X,Y,zeros(length(X1),1),'edgecolor','y'), hold on
   xlin  = linspace(min(X),max(X),20);
   ylin  = linspace(min(Y),max(Y),20);
   [XA,YA] = meshgrid(xlin,ylin);
   W     = griddata(X1,Y1,Z,XA,YA,'cubic');
   [C,h] = contour(XA,YA,W,[-10,-9,-8,-6,-4,-2,-1,-0.05],'k','linewidth',2); hold on
   contour(XA,YA,W,[0.005,0.01,0.03,0.06,0.09,0.15,0.95],'r'); hold on
   contour(XA,YA,W,[0,0],'b'); hold on
 %  contour(XA,YA,W,8,'k'); hold on

  % clabel(C,h,'labelspacing',400),  hold on
   %h     = findobj('Type','patch');
   %set(h,'Linewidth',2)
case 2, disp(' Streamslice for (U,V)')
   N1
   xlin    = linspace(min(X),max(X),50);
   ylin    = linspace(min(Y),max(Y),50);
   [XA,YA] = meshgrid(xlin,ylin);
   UA = griddata(XX,YY,U,XA,YA,'linear');
   VA = griddata(XX,YY,V,XA,YA,'linear');
   streamslice(XA,YA,UA,VA), hold on
   pp = [p(1,:),p1(1,:);p(2,:),p1(2,:)];
 %  plot(pp(1,3),pp(2,3),'k*','markersize',12), hold on
 %  plot(pp(1,48),pp(2,48),'k*','markersize',12)

case 3, disp(' Contour for P*100 ')
   P = 100*P; % !!!!!!!!!!!!!!!!!!!!!
   xlin    = linspace(min(X),max(X),50);
   ylin    = linspace(min(Y),max(Y),50);
   [XA,YA] = meshgrid(xlin,ylin);
   W = griddata(X,Y,P,XA,YA,'cubic');
   [C,h] = contour(XA,YA,W,[-1,-3,-5,-7,-9,-11,-13],'k','linewidth',2); hold on
   clabel(C,h);
   [C,h] = contour(XA,YA,W,[0,1,2,3,4,5,6,7,8],'r','linewidth',2); hold on
    %  [C,h] = contour(XA,YA,W,[12,15,20],'k'); hold on
   clabel(C,h);
case 4, disp(' pressure by post calculation with convection term ')
   NU    = 0.001;  % coeff. of viscosity [m*m/sec]
   pcond = 1;
   P = pressure(FF3,p,e,p1,t,t1,U,V,NU,pcond);
   P = P(1:N1);
      P = 100*P; % !!!!!!!!!!!!!!!!!!!!!
   xlin    = linspace(min(X),max(X),50);
   ylin    = linspace(min(Y),max(Y),50);
   [XA,YA] = meshgrid(xlin,ylin);
   W = griddata(X,Y,P,XA,YA,'cubic');
      [C,h] = contour(XA,YA,W,[-1,-3,-5,-7,-9,-11,-13],'k','linewidth',2); hold on
   clabel(C,h);
   [C,h] = contour(XA,YA,W,[0,1,2,3,4,5,6,7,8],'r','linewidth',2); hold on
    %  [C,h] = contour(XA,YA,W,[12,15,20],'k'); hold on
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
   load daten12d Z 
   ZWERT = Z(N)
end
%axis off
clear
  