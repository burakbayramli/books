function bild01b
% Image for Island in a bay
disp('Contour for z')
load daten1a p e t
STUNDE = input(' Welche STUNDE?, (1/2/3/4/5/6/7/8/9/10/11/12) ');
%STUNDE = 7
I = find(e(5,:) == 2); LI = length(I);
RAND1 = [p(1,e(1,I));p(2,e(1,I))];
RAND2 = [0          ,  0 ,  10E3, 10E3, 0,   0;
         p(2,e(2,I(LI))), 16E3, 16E3, 0,    0,  p(2,e(1,I(1)))];
RAND = [RAND1,RAND2];
X = p(1,:); Y = p(2,:); H = p(3,:); Z1 = zeros(1,length(X));
clf, hold on
xlin    = linspace(min(X),max(X),30);
ylin    = linspace(min(Y),max(Y),30);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
%trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
fill(RAND(1,:),RAND(2,:),'y','erasemode','none'), hold on
axis equal, axis manual
% -- Contour for Z -------------
switch STUNDE
case 1, disp(' 1. hour, cold start ')
   load daten1b_1 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[0.51,0.53,0.55,0.57,0.59,0.61]); hold on
%   [C,h] = contour(U1,V1,W1,6); hold on
case 2, disp(' 2. hour ')
   load daten1b_2 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[0.74,0.76,0.78,0.8,0.82,0.84]); hold on
%   [C,h] = contour(U1,V1,W1,6); hold on
case 3, disp(' 3. hour ')
   load daten1b_3 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[1.02,1.04,1.06,1.09,1.11,1.14]); hold on
 %  [C,h] = contour(U1,V1,W1,6); hold on
case 4, disp(' 4. hour ')
   load daten1b_4 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[0.85,0.855,0.86,0.865,0.87]); hold on
   %[C,h] = contour(U1,V1,W1,6); hold on
case 5, disp(' 5. hour ')
   load daten1b_5 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[0.47,0.48,0.49,0.50]); hold on
 %  [C,h] = contour(U1,V1,W1,6); hold on
case 6, disp(' 6. hour ')
   load daten1b_6 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[0.02,0.04,0.06,0.08,0.10,0.12]); hold on
   %[C,h] = contour(U1,V1,W1,6); hold on
case 7, disp(' 7. hour ')
   load daten1b_7 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-0.55,-0.5,-0.45,-0.4]); hold on
   %[C,h] = contour(U1,V1,W1,6); hold on
case 8, disp(' 8. hour ')
   load daten1b_8 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-0.75,-0.79,-0.81,-0.83,-0.85,-0.87]); hold on
   %[C,h] = contour(U1,V1,W1,6); hold on
case 9, disp(' 9. hour ')
   load daten1b_9 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-1.06,-1.04,-1.02,-1.00]); hold on
   %[C,h] = contour(U1,V1,W1,6); hold on
case 10, disp(' 10. hour ')
   load daten1b_10 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,6); hold on
case 11, disp(' 11. hour ')
   load daten1b_11 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,6); hold on
case 12, disp(' 12. hour ')
   load daten1b_12 V SHALLOW
   W1 = griddata(X,Y,V(3,:),U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,6); hold on
end
clabel(C,h,'manual','color','k')
if ~isempty(SHALLOW)
   mesh45(p,e,t,SHALLOW);
end
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
insel(p,e), hold on
fill(RAND(1,:),RAND(2,:),'y','erasemode','none'), hold on
clear
