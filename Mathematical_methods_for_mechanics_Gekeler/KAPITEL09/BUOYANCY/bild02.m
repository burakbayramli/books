function bild02
% Figures for DEMO2.M and DEMO6.M
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),

%demo_number = 100;
%while ~ismember(demo_number,[2,6])
%   demo_number = input(' Select demo_number! (2/6) ');
%end
demo_number = 6;
switch demo_number
case 2, load daten2a p e t p1 t1, load daten2b U V TT P, FF3 = 'bsp02h';
case 6, load daten6a p e t p1 t1 parmeter, load daten6b U V TT P, FF3 = 'bsp02h'; 
end

NODES = size(p,2) + size(p1,2)
bilda = 100;
while ~ismember(bilda,[1,2,3,4,5])
   bilda = input(' Select figure! (1/2/3/4/5) ');
end
%bilda = 4;
clf, hold on
minx = min(p(1,:)); miny = min(p(2,:));
maxx = max(p(1,:)); maxy = max(p(2,:));
plot(minx-0.01,miny-0.01,'.w','markersize',6), hold on
plot(maxx + 0.01,maxy + 0.01,'.w','markersize',6)
axis equal tight, axis manual

N1 = size(p,2); U1 = U(1:N1).'; V1 = V(1:N1).';
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P.';
pp = [p,p1]; XX = pp(1,:); YY = pp(2,:); 
xlin = linspace(min(X),max(X),30); ylin = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
%trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','y'), hold on
for I = 1:size(e,2)
   A = [p(1,e(1:2,I))]; B = [p(2,e(1:2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
%plot(p1(1,:),p1(2,:),'k*','markersize',6), hold on
% pause
switch bilda  
case 1, disp(' Contour of stream function Z*100 ')
   [RDU,RDV,RDT,RDP,FU,FV,FT,RDZ,RCZ] = feval(FF3,p,e,p1,parmeter);
%   LASTEN  =  rside_post(p,t,t1,U,V,RCP); % also RCP = []
    LASTEN  =  rside_post(p,t,t1,U,V,[]); % also RCP = []
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
   Z = 10*Z; %!!!!!!!!!!!!!!!!!!!
   X1 = [p(1,:),p1(1,:)];  Y1 = [p(2,:),p1(2,:)];
   xlin  = linspace(min(X),max(X),20);
   ylin  = linspace(min(Y),max(Y),20);
   [XA,YA] = meshgrid(xlin,ylin);
   W     = griddata(X1,Y1,Z,XA,YA,'cubic');
   [C,h] = contour(XA,YA,W,10,'k','linewidth',2); hold on
 %  clabel(C,h,'labelspacing',400)
   [C,h] = contour(XA,YA,W,[97,97],'k','linewidth',2); hold on

   %h     = findobj('Type','patch');
   %set(h,'Linewidth',2)
case 2, disp(' Contour for T ')
   xlin    = linspace(min(X),max(X),50);
   ylin    = linspace(min(Y),max(Y),50);
   [XA,YA] = meshgrid(xlin,ylin);
   TA = griddata(XX,YY,TT,XA,YA,'cubic');
   [C,h] = contour(XA,YA,TA,[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9],'k','linewidth',2);
   clabel(C,h,'manual')
  % contour(XA,YA,TA,10,'k', 'linewidth',2)
case 3, disp(' Contour for P*100 ')
   P = P*100; % !!!!!!!!!!!!!!!!!!!!!
   xlin    = linspace(min(X),max(X),50);
   ylin    = linspace(min(Y),max(Y),50);
   [XA,YA] = meshgrid(xlin,ylin);
   W = griddata(X,Y,P,XA,YA,'cubic');
  % [C,h] = contour(XA,YA,W,[-100,-80,-60,-40,-20,0,20,40,60 80,100]);
   %clabel(C,h);
  % [C,h] = contour(XA,YA,W,[0,1,2,3,4,5,6,7,8],'r'); hold on
      [C,h] = contour(XA,YA,W,10,'k'); hold on
   %clabel(C,h);
case 4, disp(' Streamslice for (U,V)')
   Number_of_nodes_in_p = N1
   xlin    = linspace(min(X),max(X),50);
   ylin    = linspace(min(Y),max(Y),50);
   [XA,YA] = meshgrid(xlin,ylin);
   UA = griddata(XX,YY,U,XA,YA,'linear');
   VA = griddata(XX,YY,V,XA,YA,'linear');
   streamslice(XA,YA,UA,VA), hold on
   
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
clear
  