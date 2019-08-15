function bild09
% Figures for DEMO13.M and DEMO14.M
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),

demo_number = 100;
while ~ismember(demo_number,[13,14])
   demo_number = input(' Select demo_number! (13/14) ');
end
format short, format compact

%demo_number = 13;
switch demo_number
case 13, load daten13a p e t p1 t1 FF3 parmeter, load daten13b U V P
case 14, load daten14a p e t p1 t1 FF3 parmeter, load daten14b U V P
end
scaled = parmeter(5);
bilda = 100;
while ~ismember(bilda,[1,2,3,4,5])
   bilda = input(' Select figure? (1/2/3/4/5)  ');
end
nodenumber = size(p,2) + size(p1,2)
X1 = p(1,:); Y1 = p(2,:); Z1 = zeros(1,length(X1)); P = P.';
N1 = size(p,2); U1 = U(1:N1); V1 = V(1:N1);
clf, hold on
LU = [min(X1),min(Y1)]; LU = LU - 0.01;
RO = [max(X1),max(Y1)]; RO = RO + 0.01;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(1),RO(2),'w.'), hold on

trimesh(t(1:3,:).',X1,Y1,Z1,'edgecolor','y'), hold on
out_bound = find(e(7,:) == 0);
out_bound = sort(out_bound);
E = e(:,out_bound);
for I = 1:size(E,2)
      plot(p(1,E(1:2,I)),p(2,E(1:2,I)),'r','linewidth',2), hold on
end
axis equal tight, axis manual, grid off

xlin  = linspace(min(X1),max(X1),40);
ylin  = linspace(min(Y1),max(Y1),40);
[X2,Y2] = meshgrid(xlin,ylin);

switch bilda
case 1 % Contour of stream function by postprozessor
   [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = feval(FF3,p,e,p1,parmeter);
   LASTEN  = rside_post(p,t,t1,U,V,[]);
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
  % Z = 100*Z; %!!!!!!!!!!!!!!!!!!!
   X  = [p(1,:),p1(1,:)]; Y  = [p(2,:),p1(2,:)];
   xlin  = linspace(min(X),max(X),40);
   ylin  = linspace(min(Y),max(Y),40);
   [X2,Y2] = meshgrid(xlin,ylin);
   W     = griddata(X,Y,Z,X2,Y2,'cubic');
   if demo_number == 13
      if scaled == 0
      contour(X2,Y2,W,[0,0.1,0.4,0.8,1.2,1.4,1.8,2.2,2.8],'k'); hold on
      contour(X2,Y2,W,[-0.12,-0.1,-0.07,-0.05,-0.01],'r')
      end 
      if scaled == 1
      contour(X2,Y2,W,10*[0,0.1,0.4,0.8,1.2,1.4,1.8,2.2,2.8],'k'); hold on
      contour(X2,Y2,W,[-0.25,-0.12,-0.1,-0.07,-0.05,-0.01],'r')
      %contour(X2,Y2,W,[-0.25,-0.25],'b','linewidth',2)
      end 
   end
   if demo_number == 14
      if scaled == 0
      contour(X2,Y2,W,0.1*[0,0.1,0.4,0.8,1.2,1.4,1.8,2.2,2.8],'k'); hold on
      contour(X2,Y2,W,[-0.12,-0.1,-0.07,-0.05,-0.01,-0.005],'r'), hold on
      end 
      if scaled == 1
      contour(X2,Y2,W,10*[0,0.05,0.1,0.4,0.8,1.2,1.4,1.8,2.2,2.8],'k'); hold on
      contour(X2,Y2,W,[-0.12,-0.1,-0.07,-0.05,-0.01,-0.005],'r')
      end 
      if scaled == 2
      contour(X2,Y2,W,10*[0,0.1,0.4,0.8,1.2,1.4,1.8,2.2,2.8],'k'); hold on
      contour(X2,Y2,W,[-0.25,-0.12,-0.1,-0.07,-0.05,-0.01],'r')
      %contour(X2,Y2,W,[-0.25,-0.25],'b','linewidth',2)
      end 
   end
   axis equal tight
case 2, disp(' Streamslice  ')
   U2 = griddata(X1,Y1,U1,X2,Y2,'cubic');
   V2 = griddata(X1,Y1,V1,X2,Y2,'cubic');
   streamslice(X2,Y2,U2,V2)
   %  weisseln(p,e,t) % fails because e ~= boundary
case 3, disp(' Contour for P ')
   P = P*1000;
   W = griddata(X1,Y1,P,X2,Y2,'cubic');
   contour(X2,Y2,W,10);
  % [C,h] = contour(X2,Y2,W,10,'k');
  % clabel(C,h,'manual');
case 4, disp(' pressure by post processor ')
   P = pressure_backstep(FF3,p,e,p1,t,t1,U,V,parmeter);
  % P = P*1000;
   
   P = P(1:N1);
   W = griddata(X1,Y1,P,X2,Y2,'cubic');
%   contour(X2,Y2,W,[0.25,0.5,0.75,1.0,1.25,1.5,2,2.5],'k');
%   contour(X2,Y2,W,[-4.0,-3.5,-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0],'r');
   %contour(X2,Y2,W,[0,0],'b');
   [C,h] = contour(X2,Y2,W,10,'k');
   % clabel(C,h,'manual');
case 5, disp(' flow ')
    U1 = U1.'; V1 = V1.';
    quiver(X1,Y1,U1,V1,2), hold on
case 6
   U1 = griddata(X,Y,U,X1,Y1,'cubic');
   V1 = griddata(X,Y,V,X1,Y1,'cubic');
   XSTART  = [0.05,0.1,0.2,0.3,0.4];
   YSTART  = 0.5*ones(1,length(XSTART));
   H = streamline(X1,Y1,U1,V1,XSTART,YSTART,[0.1,500]);
end
%axis off
clear all
