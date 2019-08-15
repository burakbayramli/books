function bild04a
% Image for back facing step, contour plot
% with notations
load daten4a p e t RAND Parmeter
load daten4b RDZ RDW NACHBAR NORMALEN
load daten4c200 V W Z

% -- Eckpunkte ----------------------
clf, hold on
plot(-0.025,-0.025,'w.'), hold on
plot(0.385,0.085,'w.'), hold on
axis equal tight, axis manual, grid on
% -- Rand -----------------------
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
% -- Contour --------------------------
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
%trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
W1 = griddata(X,Y,Z,U1,V1,'cubic');
[C,h] = contour(U1,V1,W1,10);
text(0.16,0.07,'(A)','fontsize',16)
text(0.16,-0.01,'(C)','fontsize',16)
text(-0.025,0.04,'(B)','fontsize',16)
text(0.36,0.03,'(D)','fontsize',16)
text(0.01,0.01,'(C)','fontsize',10)
