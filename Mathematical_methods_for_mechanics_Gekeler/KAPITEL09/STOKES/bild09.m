function bild09
% Figures for DEMO15.M
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),

load daten15a p e t p1 t1
load daten15b U V P    % full output
disp(' Figure 4 with postprozessor for stream function ')
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Select figure? (1/2/3/4)  ');
end

X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P';
clf, hold on
N1 = size(p,2);
U1 = U(1:N1); V1 = V(1:N1);
xlin  = linspace(min(X),max(X),30);
ylin  = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
W1     = griddata(X,Y,Z1,X1,Y1,'cubic');
trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','g'), hold on
for I = 1:size(e,2)
   plot(p(1,e(1:2,I)),p(2,e(1:2,I)),'r','linewidth',2), hold on
end
LU = [min(X),min(Y)]; LU = LU - 0.01;
RO = [max(X),max(Y)]; RO = RO + 0.01;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(1),RO(2),'w.'), hold on

axis equal tight, axis manual, grid off
switch bilda
case 1 % Contour of stream function by postprozessor
   % -- Generation ------
   [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = bsp09h(p,e,p1);
   LASTEN  = rside_post(p,t,t1,U,V,[]);
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
   save daten9d Z
   % --------------------
   load daten9d Z 
   Z = 100*Z; %!!!!!!!!!!!!!!!!!!!
   X1 = p(1,:);  Y1 = p(2,:);
   X2 = p1(1,:); Y2 = p1(2,:);
   X  = [X1,X2]; Y  = [Y1,Y2];
   Z1 = zeros(length(X1),1);
   xlin  = linspace(min(X),max(X),40);
   ylin  = linspace(min(Y),max(Y),40);
   [U,V] = meshgrid(xlin,ylin);
   W     = griddata(X,Y,Z,U,V,'cubic');
   [C,h] = contour(U,V,W,[5,10,15,20,25,30,35],'k'); hold on
   contour(U,V,W,[0,0],'b'); hold on
   contour(U,V,W,[-0.2,-0.5,-0.1],'r'); hold on
   
  % contour(U,V,W,10,'b'); hold on

   %clabel(C,h,'labelspacing',400),  hold on
   %h     = findobj('Type','patch');
   %set(h,'Linewidth',2)
   axis equal
case 2, disp(' Streamslice  ')
   U2 = griddata(X,Y,U1,X1,Y1,'cubic');
   V2 = griddata(X,Y,V1,X1,Y1,'cubic');
   streamslice(X1,Y1,U2,V2)
   %  weisseln(p,e,t) % fails because e ~= boundary
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
