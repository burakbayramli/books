function geometrytest
% Geometry test for shallow water problems
% example for edge matrix, bsp01.m
%e = [1, 2, 3, 4;  % indices of starting points
%     2, 3, 4, 1;  % indices of ending points
%     0, 0, 0, 0;  % left parameter value in edge
%     1, 1, 1, 1;  % right parameter value in edge 
%     1, 2, 3, 4;  % segment number
%     1, 1, 1, 1;  % left subdomain number
%     0, 0, 0, 0]; % right subdomain number (exterior domain)

clc, format short, format compact
example = 1;
switch example
case 1 
   FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp01h';
   SEGNR = [1,2,3]; % [1,2] outer boundary, 3 interior boundary
end
figure = 100; 
while ~ismember(figure,[1,2,3,4,5])
   figure = input(' Select figure (1/2/3/4/5) ');
end
% -----------------------------
REFINE = 0;
[p,e,t,waterdepth] = start4shallow(FF1,REFINE); 
bild00(p,e,t)
pause
clf
I = find(e(5,:) == 2); LI = length(I);
% Boundary for FILL.M
RAND1 = [p(1,e(1,I));p(2,e(1,I))];
RAND2 = [0          ,  0 ,  10E3, 10E3, 0,   0;
         p(2,e(2,I(LI))), 16E3, 16E3, 0,    0,  p(2,e(1,I(1)))];
RAND = [RAND1,RAND2];

X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
hold on  % fuer flaches Bild --------
xlin    = linspace(min(X),max(X),30);
ylin    = linspace(min(Y),max(Y),30);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'v4');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
axis equal, axis manual
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end

clf, hold on
trimesh(t(1:3,:).',p(1,:),p(2,:),zeros(1,size(p,2)),'edgecolor','g')
hold on
axis equal, grid on

switch figure
case 1, disp('waterdepth ')
case 2, disp(' Check triangles and subdomains ') 
   SUBDOMAIN_NUMBER = max(t(4,:));
   for I = 1:SUBDOMAIN_NUMBER
      I;
      SUBDOMAIN = find(t(4,:) == I);
      for K = 1:length(SUBDOMAIN)
         J = t(1:3,SUBDOMAIN(K));
         fill(p(1,J),p(2,J),'g','erasemode','none'), hold on
         pause(0.2) 
      end
   end
case 3, disp(' Check whether boundary is ordered for normals ')
   LS = length(SEGNR);
   AUX = [SEGNR,SEGNR(1)]; % closed boundary  
   for I = 1:LS
      K = find(e(5,:) == AUX(I)); LK = length(K); 
      L = find(e(5,:) == AUX(I+1)); LL = length(L); 
      P1 = plot(p(1,e(2,K(LK))),p(2,e(2,K(LK))),'k*'); hold on
      P2 = plot(p(1,e(1,L(1))) ,p(2,e(1,L(1))),'ro');
      pause
      delete(P1), delete(P2)
   end
   
case 4, disp(' Check segment numbers ')
   LS = max(e(5,:))
 %  for I = 1:LS
   for I = 3
      I 
      K = find(e(5,:) == I); 
      SEGMENT = e(:,K);
      LSEG = size(SEGMENT,2); 
      for L = 1:LSEG
      P1 = plot(p(1,SEGMENT(1:2,L)),p(2,SEGMENT(1:2,L)),'r'); hold on
      pause
      delete(P1)
      end
   end
case 5, disp(' Check tangents and normals of boundary ')
   SEGNR = [1,2];
   SEGNR = 3;
   LL = 1000; % length for visualizing 
   TANGENTS = lanscape(p,e,SEGNR);
   %[TANGENTS,NORMALS] = tang_norm(p,e,SEGNR);
   for I = 1:size(TANGENTS,2)
   I  
   AUX = TANGENTS(1,I);
   XA = p(1,AUX); YA = p(2,AUX);
   plot(XA,YA,'k*'), hold on
   XB = [XA; XA + LL*TANGENTS(2,I)]; YB = [YA; YA + LL*TANGENTS(3,I)];
      plot(XB,YB,'r','linewidth',2), hold on
      pause(0.2)
   end
   pause
   for I = 1:size(NORMALS,2)
      I  
      AUX = NORMALS(1,I);
      XA = p(1,AUX); YA = p(2,AUX);
      plot(XA,YA,'k*'), hold on
      XB = [XA; XA + LL*NORMALS(2,I)]; YB = [YA; YA + LL*NORMALS(3,I)];
      plot(XB,YB,'r','linewidth',2), hold on
      pause(0.2)
   end
case 6
   for I = 1:size(t,2)
      J = t(1:3,I);
      XA = p(1,J); YA = p(2,J);
      fill(XA,YA,'g'), hold on
      pause(0.1)
   end

end

