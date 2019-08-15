function bild03
% Figures for Benhard cell
% Contour of stream function with post prozessor
% Streamslice for (U,V), CONTOUR for P, QUIVER for (U,V),
demo_number = 100;
while ~ismember(demo_number,[3,7])
   demo_number = input(' Select demo_number! (3/7) ');
end
%demo_number = 7;
switch demo_number
case 3, load daten3a p e t p1 t1 parmeter_h,
        load daten3b U V TT P, FF3 = 'bsp03h';
case 7, load daten7a p e t p1 t1 parmeter_h
        load daten7b U V TT P, FF3 = 'bsp03h'; 
end
segnr_a  = [37,34,31,28,25,22,19,16,13,10,7,3]; %above
segnr_b = 4;                                    %left 
segnr_c = [1,5,8,11,14,17,20,23,26,29,32,35];   %below
segnr_d = 36;                                   % right
segnr = [segnr_a,segnr_b,segnr_c,segnr_d]; 

bilda = 100;
while ~ismember(bilda,[1,2,3,4,5])
   bilda = input(' Select figure! (1/2/3/4/5) ');
end
%bilda = 4;
clf, hold on
ee = 0.1;
ee = 0;
minx = min(p(1,:)); miny = min(p(2,:));
maxx = max(p(1,:)); maxy = max(p(2,:));
plot((1-ee)*minx,(1-ee)*miny,'.w','markersize',6), hold on
plot((1+ee/10)*maxx,(1+ee)*maxy,'.w','markersize',6)
axis equal tight, axis manual

N1 = size(p,2); U1 = U(1:N1).'; V1 = V(1:N1).';
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P.';
pp = [p,p1]; XX = pp(1,:); YY = pp(2,:); 
xlin = linspace(min(X),max(X),30); ylin = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
%trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','y'), hold on
%for I = 1:size(e,2)
%   A = [p(1,e(1:2,I))]; B = [p(2,e(1:2,I))];
%   plot(A,B,'r','linewidth',2), hold on
%end
switch bilda  
case 1, disp(' Contour of stream function Z*100 ')
    [RDU,RDV,RDT,RDP,FU,FV,FT,RDZ,RCZ] = feval(FF3,p,e,p1,parmeter_h);
%   LASTEN  =  rside_post(p,t,t1,U,V,RCP); % also RCP = []
    LASTEN  =  rside_post(p,t,t1,U,V,[]); % also RCP = []
   Z       = ellipt_post(p,t,p1,t1,RDZ,RCZ,LASTEN);
   Z = 100*Z; %!!!!!!!!!!!!!!!!!!!
   X1 = [p(1,:),p1(1,:)];  Y1 = [p(2,:),p1(2,:)];
   xlin  = linspace(min(X),max(X),20);
   ylin  = linspace(min(Y),max(Y),20);
   [XA,YA] = meshgrid(xlin,ylin);
   W     = griddata(X1,Y1,Z,XA,YA,'cubic');
 %  [C,h] = contour(XA,YA,W,[-10,-9,-8,-6,-4,-2,-1,-0.05],'k'); hold on
 %  contour(XA,YA,W,[0.005,0.01,0.03,0.06,0.09,0.15,0.95],'r'); hold on
 %  contour(XA,YA,W,[0,0],'b'); hold on
   contour(XA,YA,W,8,'k'); hold on
   axis equal tight
case 2, disp(' Contour for T ')
   xlin    = linspace(min(X),max(X),50);
   ylin    = linspace(min(Y),max(Y),50);
   [XA,YA] = meshgrid(xlin,ylin);
   TA = griddata(XX,YY,TT,XA,YA,'cubic');
  % [C,h] = contour(XA,YA,TA,[3.4,3.39,3.38,3.37,3.36,3.35,3.34,3.33],'k'); hold on
  % clabel(C,h,'labelspacing',1000)
   contour(XA,YA,TA,10,'k')
case 3, disp(' Contour for P*100 ')
   P = 1000*P; % !!!!!!!!!!!!!!!!!!!!!
   xlin    = linspace(min(X),max(X),50);
   ylin    = linspace(min(Y),max(Y),50);
   [XA,YA] = meshgrid(xlin,ylin);
   W = griddata(X,Y,P,XA,YA,'cubic');
 %  [C,h] = contour(XA,YA,W,[-1,-2,-3,-4,-5,-6,-7,-8],'b'); hold on
 %  clabel(C,h);
 %  [C,h] = contour(XA,YA,W,[0,1,2,3,4,5,6,7,8],'r'); hold on
    %  [C,h] = contour(XA,YA,W,[12,15,20],'k'); hold on
  % clabel(C,h);
   contour(XA,YA,W,20,'k')
   
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
   %load daten12d Z 
   %ZWERT = Z(N)
end
clear
  