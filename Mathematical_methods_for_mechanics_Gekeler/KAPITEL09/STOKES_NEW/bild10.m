function bild10
% figures for DEMO11.M
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Select figure!, (1/2/3/4) ');
end

%bilda = 4;
load daten11a p e t p1 t1 %RAND
load daten11b U V P nu FF3 U0
node_number = size(p,2) + size(p1,2)

X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P';
clf, hold on
N1 = size(p,2);
U2 = U(1:N1); V2 = V(1:N1);
xlin  = linspace(min(X),max(X),30);
ylin  = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
W1     = griddata(X,Y,Z1,X1,Y1,'cubic');
%trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','y'), hold on
out_bound = find(e(7,:) == 0);
out_bound = sort(out_bound);
E = e(:,out_bound);

for I = 1:size(E,2)
      plot(p(1,E(1:2,I)),p(2,E(1:2,I)),'r','linewidth',2), hold on
end
LU = [min(X),min(Y)]; LU = LU - 0.1;
RO = [max(X),max(Y)]; RO = RO + 0.1;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(1),RO(2),'w.'), hold on
axis equal tight, axis manual, grid off
switch bilda
case 1 % Contour of stream function by postprozessor
   [RDU,RDV,RDP,FU,FV,RDZ,RCZ] = feval(FF3,p,e,p1,U0);
   LASTEN  = rside_post(p,t,t1,U,V,[]);
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
   X1 = p(1,:);  Y1 = p(2,:);
   X2 = p1(1,:); Y2 = p1(2,:);
   X  = [X1,X2]; Y  = [Y1,Y2];
   Z1 = zeros(length(X1),1);
   xlin  = linspace(min(X),max(X),40);
   ylin  = linspace(min(Y),max(Y),40);
   [U,V] = meshgrid(xlin,ylin);
   W     = griddata(X,Y,Z,U,V,'linear');
   contour(U,V,W,[-3,-2,-1,-0.5,0.5,1,2,3],'k','linewidth',2); hold on
   contour(U,V,W,[0,0],'b','linewidth',2); hold on
  % clabel(C,h,'manual'),  hold on
   contour(U,V,W,[0.01,0.02,0.04,0.06,0.08],'r'); hold on
   contour(U,V,W,[-0.01,-0.02,-0.04,-0.06,-0.08],'r'); hold on

case 2, disp(' Streamslice  ')
   U1 = griddata(X,Y,U2,X1,Y1,'cubic');
   V1 = griddata(X,Y,V2,X1,Y1,'cubic');
   streamslice(X1,Y1,U1,V1)
   %  weisseln(p,e,t) % fails because e ~= boundary
case 3, disp(' Contour for P ')
   W = griddata(X,Y,P,X1,Y1,'cubic');
   contour(X1,Y1,W,[0.2,0.4,0.6,0.8,1.0,1.2,1.4],'k','linewidth',2);
   contour(X1,Y1,W,[-0.8,-0.7,-0.6,-0.5,-0.45,-0.4,-0.3,-0.2,-0.1],'r','linewidth',2);
   contour(X1,Y1,W,[0,0],'b','linewidth',2); 
   %contour(X1,Y1,W,[-0.45,-0.45],'g');

   % [C,h] = contour(X1,Y1,W,10);
    %clabel(C,h,'manual');
    % fill ball
    TT = linspace(0,2*pi,40);
    R1 = 7 + 2*cos(TT); R2 = 5 + 2*sin(TT);
    fill(R1,R2,'y','erasemode','none') 
case 4, disp(' pressure by post processor ')
   nu = 0.1;  % coeff. of viscosity [m*m/sec]
   P = pressure(FF3,p,e,p1,t,t1,U,V,nu);
   P = P(1:N1);
   W = griddata(X,Y,P,X1,Y1,'cubic');
   contour(X1,Y1,W,[0.2,0.4,0.6,0.8,1.0,1.2,1.4],'k','linewidth',2);
   contour(X1,Y1,W,[-0.8,-0.7,-0.6,-0.5,-0.45,-0.4,-0.3,-0.2,-0.1],'r','linewidth',2);
   contour(X1,Y1,W,[0,0],'b','linewidth',2); 
   %contour(X1,Y1,W,[0,0],'b');
   %[C,h] = contour(X1,Y1,W,10);
   % clabel(C,h,'manual');
        % fill ball
    TT = linspace(0,2*pi,40);
    R1 = 7 + 2*cos(TT); R2 = 5 + 2*sin(TT);
    fill(R1,R2,'y','erasemode','none') 

case 5, disp(' flow ')
   U2 = U2.'; V2 = V2.';
   quiver(X,Y,U2,V2,2), hold on
case 6 % Find value of a special point
   N = find_number(p,t);   
   load daten10d Z 
   ZWERT = Z(N)
end
axis off
clear