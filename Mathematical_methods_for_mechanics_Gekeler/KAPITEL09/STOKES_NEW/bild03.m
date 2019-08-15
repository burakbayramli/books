function bild03
% Figures for DEMO3.M
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),

load daten3a p e t p1 t1 U0 FF3, load daten3b U V P 
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Select figure! (1/2/3/4) ');
end
%bilda = 1;
clf, hold on
X1 = p(1,:); Y1 = p(2,:);  
N1 = size(p,2); U1 = U(1:N1); V1 = V(1:N1); 
X = [p(1,:),p1(1,:)]; Y  = [p(2,:),p1(2,:)];

LU = [min(X1),min(Y1)]; LU = LU - 1/2;
RO = [max(X1),max(Y1)]; RO = RO + 1/2;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(1),RO(2),'w.'), hold on

trimesh(t(1:3,:).',X1,Y1,zeros(1,length(X1)),'edgecolor','g'), hold on
E = find(e(7,:) == 0); E = e(:,E);  % E = outer boundary segments
for I = 1:size(E,2)
      plot(p(1,E(1:2,I)),p(2,E(1:2,I)),'r','linewidth',2), hold on
end
axis equal tight, axis manual, grid off

switch bilda
case 1
   [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = feval(FF3,p,e,p1,U0);
   LASTEN  = rside_post(p,t,t1,U,V,[]);
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
   Z = 100*Z; %!!!!!!!!!!!!!!!!!!!
   xlin  = linspace(min(X),max(X),40);
   ylin  = linspace(min(Y),max(Y),40);
   [XA,YA] = meshgrid(xlin,ylin);
   W     = griddata(X,Y,Z,XA,YA,'linear');
   [C,h] = contour(XA,YA,W,10); hold on
   contour(XA,YA,W,[0,0],'r'); hold on
case 2, disp(' Streamslice  ')
   xlin  = linspace(min(X1),max(X1),30);
   ylin  = linspace(min(Y1),max(Y1),30);
   [X2,Y2] = meshgrid(xlin,ylin);
   U1 = griddata(X1,Y1,U1,X2,Y2,'cubic');
   V1 = griddata(X1,Y1,V1,X2,Y2,'cubic');
   streamslice(X2,Y2,U1,V1)
   %weisseln(p,e,t)
case 3, disp(' Contour for P ')
   xlin  = linspace(min(X1),max(X1),30);
   ylin  = linspace(min(Y1),max(Y1),30);
   [X2,Y2] = meshgrid(xlin,ylin);
   W = griddata(X1,Y1,P.',X2,Y2,'cubic');
   contour(X2,Y2,W,10,'k');
  % clabel(C,h,'manual');
case 4, disp(' flow ')
    U1 = U1.'; V1 = V1.';
    quiver(X1,Y1,U1,V1,2), hold on
end
axis off
clear
