function bild07
% Figures for DEMO11.M and DEMO12.M
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),
demo_number = 100;
%while ~ismember(demo_number,[11,12])
%   demo_number = input(' Select demo_number! (11/12) ');
%end
demo_number = 11;
switch demo_number
case 11, load daten11a p e t p1 t1, load daten11b U V P ;  
case 12, load daten12a p e t p1 t1, load daten12b U V P 
end
FF3 = 'bsp07h';
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Select figure! (1/2/3/4) ');
end

X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P';
clf, hold on
N1 = size(p,2);
U2 = U(1:N1); V2 = V(1:N1);
xlin = linspace(min(X),max(X),30);
ylin = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
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
case 1, disp(' Contour of stream function by postprozessor ')
   [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = feval(@bsp07h,p,e,p1);
   LASTEN  = rside_post(p,t,t1,U,V,[]);
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
   save daten7d Z
   load daten7d Z 
   Z = 100*Z; %!!!!!!!!!!!!!!!!!!!
   X1 = p(1,:);  Y1 = p(2,:);
   X2 = p1(1,:); Y2 = p1(2,:);
   X  = [X1,X2]; Y  = [Y1,Y2];
   Z1 = zeros(length(X1),1);
   xlin  = linspace(min(X),max(X),40);
   ylin  = linspace(min(Y),max(Y),40);
   [U,V] = meshgrid(xlin,ylin);
   W     = griddata(X,Y,Z,U,V,'linear');
   contour(U,V,W,10*[0.005,0.01,0.05,0.1,0.2,0.3,0.4,0.5],'k'); hold on
   contour(U,V,W,[0.1e-02,0.1e-02],'b'); hold on
   contour(U,V,W,[-0.01,-0.05,-0.1,-0.3,-0.5],'r'); hold on
   %clabel(C,h,'labelspacing',400),  hold on
   %h     = findobj('Type','patch');
   %set(h,'Linewidth',2)
case 2, disp(' Streamslice  ')
   U1 = griddata(X,Y,U2,X1,Y1,'cubic');
   V1 = griddata(X,Y,V2,X1,Y1,'cubic');
   streamslice(X1,Y1,U1,V1)
   %  weisseln(p,e,t) % fails because e ~= boundary
case 3, disp(' Contour for P ')
   P = P*1000; % !!!!!!!!!!!
   W = griddata(X,Y,P,X1,Y1,'cubic');
   contour(X1,Y1,W,[-6,-4,-1,10,15,20,25],'k');
   contour(X1,Y1,W,[-0.5,-8,-7],'r');
   contour(X1,Y1,W,[0,0],'b');

   % [C,h] = contour(X1,Y1,W,10);
  %  clabel(C,h,'manual');
case 4, disp(' flow ')
   U2 = U2.'; V2 = V2.';
   quiver(X,Y,U2,V2,2), hold on
end
axis off
