function bild01a
% Draws CONTOUR for P and contour of stream function
% with postprozessor in unit square

clf, hold on
load daten1 p e t p1 t1 U V P
N1 = size(p,2);
U1 = U(1:N1).'; V1 = V(1:N1).';
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P.';
xlin    = linspace(min(X),max(X),30);
ylin    = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,X1,Y1,'cubic');
bilda = 100;
%while ~ismember(bilda,[1,2])
%   bilda = input(' Which figure? (1/2) ');
%end
bilda = 2;
switch bilda
case 1
   hold on
   P = 1000*P; % !!!!!!!!!!!!!!!!!!!!!!!!!
   trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','g'), hold on
   for I = 1:size(e,2)
      A = [p(1,e(1,I));p(1,e(2,I))];
      B = [p(2,e(1,I));p(2,e(2,I))];
      plot(A,B,'r','linewidth',2), hold on
   end
   axis equal, axis manual
   W = griddata(X,Y,P,X1,Y1,'cubic');
   % Contours for OPTION = 2 and REFINE = 5
   [C,h] = contour(X1,Y1,W,[-1,-2,-3,-4,-5,-6,-7,-8],'b'); hold on
  % clabel(C,h);
   [C,h] = contour(X1,Y1,W,[0,1,2,3,4,5,6,7,8],'r'); hold on
  % clabel(C,h);
  % [C,h] = contour(X1,Y1,W,20,'k'); hold on
  % clabel(C,h)
  title(' Pressure times 1000 ')
case 2  
  hold on
  trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','g'), hold on
  for I = 1:size(e,2)
     A = [p(1,e(1,I));p(1,e(2,I))];
     B = [p(2,e(1,I));p(2,e(2,I))];
     plot(A,B,'r','linewidth',2), hold on
  end
  axis equal, axis manual
   [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = bsp01h(p,e,p1);
   LASTEN  = rside_post(p,t,t1,U,V);
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
   X1 = p(1,:);  Y1 = p(2,:);
   X2 = p1(1,:); Y2 = p1(2,:);
   X  = [X1,X2]; Y  = [Y1,Y2];
   xlin  = linspace(min(X),max(X),20);
   ylin  = linspace(min(Y),max(Y),20);
   [U,V] = meshgrid(xlin,ylin);
   W     = griddata(X,Y,Z,U,V,'cubic');
   [C,h] = contour(U,V,W,8,'k');
  % clabel(C,h,'labelspacing',400),  hold on
     [C,h] = contour(U,V,W,[0,0],'r');

   %h     = findobj('Type','patch');
   %set(h,'Linewidth',2)
   title(' Streamlines ')
   axis equal
end   
clear all
