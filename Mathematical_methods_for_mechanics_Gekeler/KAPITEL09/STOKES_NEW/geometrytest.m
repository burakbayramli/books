function geometrytest
% Geometry test for Taylor-Hood elements
% example for edge matrix, bsp01.m
%e = [1, 2, 3, 4;  % indices of starting points
%     2, 3, 4, 1;  % indices of ending points
%     0, 0, 0, 0;  % left parameter value in edge
%     1, 1, 1, 1;  % right parameter value in edge 
%     1, 2, 3, 4;  % segment number
%     1, 1, 1, 1;  % left subdomain number
%     0, 0, 0, 0]; % right subdomain number (exterior domain)

clc, format short, format compact
example = 2;
switch example
case 1 
   FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp01h_2';
   SEGNR = [1,2,3,4]; % outer boundary
case 2   
   FF1 = 'bsp09'; FF2 = 'bsp09g'; FF3 = 'bsp09h';
   SEGNR  = [6 7 8 9 14 19 24 29 34 39 44 45 42 43 38 33 28 23 18 13 3 5];
case 3
   FF1 = 'bsp10b'; FF2 = 'bsp10gb'; FF3 = 'bsp10h';
   SEGNR = [1,7,8,9,3,4]; % outer boundary
   SEGNR2 = [5,6];  % interior boundary
end
figure = 100; 
while ~ismember(figure,[1,2,3,4,5,6])
   figure = input(' Select figure (1/2/3/4/5/6) ');
end
clf
% -----------------------------
OPTION_MESH = 1; REFINE = 0;

[p,e,t,out_bound] = start4stokes(FF1,FF2,OPTION_MESH,REFINE); 

disp(' generation of intermediate nodes ')
[p1,e,t1]  = mesh06_t(p,e,t);
clf, hold on
trimesh(t(1:3,:).',p(1,:),p(2,:),zeros(1,size(p,2)),'edgecolor','g')
hold on
axis equal, grid on
pp = [p,p1];
switch figure
case 1
   for I = 1:size(t,2)
      J = t(1:3,I);
      XA = pp(1,J); YA = pp(2,J);
      fill(XA,YA,'g'), hold on
      K = t1(1:3,I);
      plothandle = plot(pp(1,K),pp(2,K),'r.'); hold on
      pause
      delete(plothandle)
   end
case 2, disp(' Check intermediate points ')
   for I = 1:length(e)
      A = pp(1,e(1:2,I)); B = pp(2,e(1:2,I));
      plot(A,B,'r','linewidth',2); hold on
      plothandle = plot(pp(1,e(8,I)),pp(2,e(8,I)),'k*'); hold on
      pause(0.2)
      delete(plothandle)
      %set(gca,'NextPlot','replace');
   end
case 3, disp(' Check triangles and subdomains ') 
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
case 4, disp(' Check whether boundary is ordered for normals ')
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
   
case 5, disp(' Check segment numbers ')
   LS = max(e(5,:))
   for I = 1:LS
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
   
case 6, disp(' Check normals of boundary ')
   NORMALEN = normals1(p,e,p1,SEGNR);
   NORMALEN(:,33) = [NORMALEN(1,33);0;-1];
   pp = [p,p1]; LL = 0.1;
   for I = 1:size(NORMALEN,2)
      I  
      AUX = NORMALEN(1,I);
      XA = pp(1,AUX); YA = pp(2,AUX);
      plot(XA,YA,'k*'), hold on
      XB = [XA; XA + LL*NORMALEN(2,I)]; YB = [YA; YA + LL*NORMALEN(3,I)];
      plot(XB,YB,'r','linewidth',2), hold on
      pause(0.2)
   end
end

