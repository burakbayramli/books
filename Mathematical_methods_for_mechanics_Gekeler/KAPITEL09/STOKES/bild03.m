function bild03
% Figures for DEMO3.M
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),

load daten3a p e t p1 t1
load daten3b U V P 
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Select figure! (1/2/3/4) ');
end
%bilda = 1;
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P.';
N1 = size(p,2);
U1 = U(1:N1); V1 = V(1:N1);
xlin  = linspace(min(X),max(X),30);
ylin  = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
trimesh(t(1:3,:).',X,Y,zeros(1,length(X)),'edgecolor','g'), hold on
for I = 1:size(e,2)
      plot(p(1,e(1:2,I)),p(2,e(1:2,I)),'r','linewidth',2), hold on
end
LU = [min(X),min(Y)]; LU = LU - 1;
RO = [max(X),max(Y)]; RO = RO + 1;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(1),RO(2),'w.'), hold on
axis equal tight, axis manual, grid off
switch bilda
case 1
   clf, hold on 
   [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = bsp03h(p,e,p1);
   LASTEN  = rside_post(p,t,t1,U,V,[]);
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
   save daten6c Z
   %load daten6c Z 
   Z = 100*Z; %!!!!!!!!!!!!!!!!!!!
   X1 = p(1,:);  Y1 = p(2,:);
   X2 = p1(1,:); Y2 = p1(2,:);
   X  = [X1,X2]; Y  = [Y1,Y2];
   Z1 = zeros(length(X1),1);
   trimesh(t(1:3,:).',X1,Y1,Z1,'edgecolor','y'), hold on
   xlin  = linspace(min(X),max(X),40);
   ylin  = linspace(min(Y),max(Y),40);
   [U,V] = meshgrid(xlin,ylin);
   W     = griddata(X,Y,Z,U,V,'linear');
   out_bound = find(e(7,:) == 0); out_bound = e(:,out_bound);
   plot(p(1,out_bound(1:2,:)),p(2,out_bound(1:2,:)),'r','linewidth',2), hold on
   %contour(U,V,W,'k','linewidth',1); hold on
   AUX = linspace(0,5,10); AUX = 100*AUX.^2/2; 
   AUX = AUX(2:end); % inflow Z;
   [C,h] = contour(U,V,W,[AUX],'k','linewidth',1); hold on
   NN = 10*0.25;
   contour(U,V,W,[NN,NN],'r'); hold on
 %  contour(U,V,W,[0.7e-004,0.7e-004],'b'); hold on
   axis equal
case 2, disp(' Streamslice  ')
   U1 = griddata(X,Y,U1,X1,Y1,'cubic');
   V1 = griddata(X,Y,V1,X1,Y1,'cubic');
   streamslice(X1,Y1,U1,V1)
   %weisseln(p,e,t)
case 3, disp(' Contour for P ')
   W = griddata(X,Y,P,X1,Y1,'cubic');
   contour(X1,Y1,W,10);
  % [C,h] = contour(X1,Y1,W,10);
  % clabel(C,h,'manual');
case 4, disp(' flow ')
    U1 = U1.'; V1 = V1.';
    quiver(X,Y,U1,V1,2), hold on
end
axis off
clear
