function bild07
% Figures for DEMO9.M and DEMO10.M
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),
demo_number = 100;
while ~ismember(demo_number,[9,10])
   demo_number = input(' Select demo_number! (9/10) ');
end
%demo_number = 10;
switch demo_number
case 9, load daten9a p e t p1 t1 FF3 U0,   load daten9b U V P; 
case 10, load daten10a p e t p1 t1 FF3 U0, load daten10b U V P ;

end
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Select figure! (1/2/3/4) ');
end
node_number = size(p,2) + size(p1,2)
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P';
clf, hold on
N1 = size(p,2);
U2 = U(1:N1); V2 = V(1:N1);
xlin = linspace(min(X),max(X),30);
ylin = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
%trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','y'), hold on
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
   [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = feval(FF3,p,e,p1,U0);
   LASTEN  = rside_post(p,t,t1,U,V,[]);
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
   save datenz Z
   load datenz Z 
   Z = 100*Z; %!!!!!!!!!!!!!!!!!!!
   X1 = p(1,:);  Y1 = p(2,:);
   X2 = p1(1,:); Y2 = p1(2,:);
   X  = [X1,X2]; Y  = [Y1,Y2];
   xlin  = linspace(min(X),max(X),40);
   ylin  = linspace(min(Y),max(Y),40);
   [U,V] = meshgrid(xlin,ylin);
   W     = griddata(X,Y,Z,U,V,'linear');
   contour(U,V,W,10*[0.005,0.01,0.05,0.1,0.2,0.3,0.5,1,1.5],'k','linewidth',1.5); hold on
   contour(U,V,W,[0,0],'b'); hold on
   contour(U,V,W,[-0.01,-0.05,-0.1,-0.3,-1,-2,-3,-4,-5,-6],'r'); hold on
   %clabel(C,h,'labelspacing',400),  hold on
   %h     = findobj('Type','patch');
   %set(h,'Linewidth',2)
case 2, disp(' Streamslice  ')
   U1 = griddata(X,Y,U2,X1,Y1,'cubic');
   V1 = griddata(X,Y,V2,X1,Y1,'cubic');
   streamslice(X1,Y1,U1,V1)
   %  weisseln(p,e,t) % fails because e ~= boundary
case 3, disp(' Contour for P ')
   P = P*100; % !!!!!!!!!!!
   W = griddata(X,Y,P,X1,Y1,'cubic');
   contour(X1,Y1,W,[0.25,0.5,0.75,1.0,1.25,1.5,2,2.5],'k','linewidth',1.5);
   contour(X1,Y1,W,[-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0],'r','linewidth',1.5);
   %contour(X1,Y1,W,[0,0],'b');
   %[C,h] = contour(X1,Y1,W,10,'k','linewidth',1);
   % clabel(C,h,'manual');
   % clabel(C,h);
   out_bound = find(e(7,:) == 0);
   out_bound = sort(out_bound);
   out_bound = e(:,out_bound);

   weisseln(p,out_bound,t) 
case 4, disp(' pressure by post processor ')
   NU = 0.0001;  % coeff. of viscosity [m*m/sec]
   P = pressure(FF3,p,e,p1,t,t1,U,V,NU);
   P = P*100;
   P = P(1:N1);
   W = griddata(X,Y,P,X1,Y1,'cubic');
   contour(X1,Y1,W,[0.25,0.5,0.75,1.0,1.25,1.5,2,2.5],'k');
   contour(X1,Y1,W,[-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0],'r');
   %contour(X1,Y1,W,[0,0],'b');
   %[C,h] = contour(X1,Y1,W,10);
   % clabel(C,h,'manual');

case 5, disp(' flow ')
   U2 = U2.'; V2 = V2.';
   quiver(X,Y,U2,V2,2), hold on
end
%axis off
