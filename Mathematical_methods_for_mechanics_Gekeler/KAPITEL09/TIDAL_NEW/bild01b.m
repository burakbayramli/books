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
fill(RAND(1,:),RAND(2,:),'y','erasemode','none'), hold on
axis equal, axis manual
% -- Contour for Z -------------
switch STUNDE
case 1, disp(' 1. hour, cold start '), load daten1b_1 V SHALLOW
case 2, disp(' 2. hour '), load daten1b_2 V SHALLOW
case 3, disp(' 3. hour '), load daten1b_3 V SHALLOW
case 4, disp(' 4. hour '), load daten1b_4 V SHALLOW
case 5, disp(' 5. hour '), load daten1b_5 V SHALLOW
case 6, disp(' 6. hour '), load daten1b_6 V SHALLOW
case 7, disp(' 7. hour '), load daten1b_7 V SHALLOW
case 8, disp(' 8. hour '), load daten1b_8 V SHALLOW
case 9, disp(' 9. hour '), load daten1b_9 V SHALLOW
case 10, disp(' 10. hour '), load daten1b_10 V SHALLOW
case 11, disp(' 11. hour '), load daten1b_11 V SHALLOW
case 12, disp(' 12. hour '), load daten1b_12 V SHALLOW
end
NN = linspace(min(V(3,:)),max(V(3,:)),10);
W1 = griddata(X,Y,V(3,:),U1,V1,'cubic');
contourf(U1,V1,W1,NN); hold on
MINZ_MAXZ = [min(V(3,:)),max(V(3,:))] 

%clabel(C,h,'manual','color','k')
if ~isempty(SHALLOW)
   shoal(p,e,t,SHALLOW);
end
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
island(p,e), hold on
fill(RAND(1,:),RAND(2,:),'y','erasemode','none'), hold on
clear all
