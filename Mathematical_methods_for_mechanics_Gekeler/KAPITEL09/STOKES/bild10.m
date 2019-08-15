function bild10
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Select figure!, (1/2/3/4) ');
end
%bilda = 4;
load daten13a p e t p1 t1 %RAND
load daten13b U V P 

X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P';
clf, hold on
N1 = size(p,2);
U2 = U(1:N1); V2 = V(1:N1);
xlin  = linspace(min(X),max(X),30);
ylin  = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
W1     = griddata(X,Y,Z1,X1,Y1,'cubic');
trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','y'), hold on
for I = 1:size(e,2)
      plot(p(1,e(1:2,I)),p(2,e(1:2,I)),'r','linewidth',2), hold on
end
LU = [min(X),min(Y)]; LU = LU - 0.1;
RO = [max(X),max(Y)]; RO = RO + 0.1;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(1),RO(2),'w.'), hold on
axis equal tight, axis manual, grid off
switch bilda
case 1 % Contour of stream function by postprozessor
   newstart = 1; 
   if newstart == 1
      [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = bsp10h(p,e,p1);
      LASTEN  = rside_post(p,t,t1,U,V,[]);
      Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
      save daten10d Z
   end
   load daten10d Z 
   %Z = 100*Z; %!!!!!!!!!!!!!!!!!!!
   X1 = p(1,:);  Y1 = p(2,:);
   X2 = p1(1,:); Y2 = p1(2,:);
   X  = [X1,X2]; Y  = [Y1,Y2];
   Z1 = zeros(length(X1),1);
   xlin  = linspace(min(X),max(X),40);
   ylin  = linspace(min(Y),max(Y),40);
   [U,V] = meshgrid(xlin,ylin);
   W     = griddata(X,Y,Z,U,V,'linear');
   [C,h] = contour(U,V,W,[-16,-12,-8,-4,0,4,8,12,16],'k'); hold on
   contour(U,V,W,[-2,-1,1,2],'b'); hold on
   %contour(U,V,W,[-100.1,0.2,0.3,0.4,-0.5],'r'); hold on
  % clabel(C,h,'manual'),  hold on
   contour(U,V,W,[0.26,-0.005,0.01,0.05],'r'); hold on
   contour(U,V,W,[0.23,0.23],'k'); hold on

   %h     = findobj('Type','patch');
   %set(h,'Linewidth',2)
   %pause
case 2, disp(' Streamslice  ')
   U1 = griddata(X,Y,U2,X1,Y1,'cubic');
   V1 = griddata(X,Y,V2,X1,Y1,'cubic');
   streamslice(X1,Y1,U1,V1)
   %  weisseln(p,e,t) % fails because e ~= boundary
case 3, disp(' Contour for P ')
   P = P*1000; % !!!!!!!!!!!
   W = griddata(X,Y,P,X1,Y1,'cubic');
   %contour(X1,Y1,W,[-6,-4,-1,10,15,20,25],'k');
   %contour(X1,Y1,W,[-0.5,-8,-7],'r');
   %contour(X1,Y1,W,[0,0],'b');
   contour(X1,Y1,W,10,'k');

   % [C,h] = contour(X1,Y1,W,10);
  %  clabel(C,h,'manual');
case 4, disp(' flow ')
   U2 = U2.'; V2 = V2.';
   quiver(X,Y,U2,V2,2), hold on
case 5 % Find value of a special point
   N = find_number(p,t);   
   load daten10d Z 
   ZWERT = Z(N)
end
axis off
clear