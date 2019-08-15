function bild04b
% Image for back facing step, contour plot
% with notations
load daten4a p e E t Parmeter
load daten4b RDZ RDW NACHBAR NORMALEN
load daten4c100 V W Z, ZA = Z;
load daten4c200 V W Z, ZB = Z;
load daten4c300 V W Z, ZC = Z;
load daten4c400 V W Z, ZD = Z;
load daten4c500 V W Z, ZE = Z;

X = p(1,:); Y = p(2,:);
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
clf, hold on
subplot(5,1,1)
for I = 1:size(E,2)
   A = [p(1,E(1,I));p(1,E(2,I))];
   B = [p(2,E(1,I));p(2,E(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal tight, axis off
W1 = griddata(X,Y,ZA,U1,V1,'cubic');
[C,h] = contour(U1,V1,W1,10);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(5,1,2)
for I = 1:size(E,2)
   A = [p(1,E(1,I));p(1,E(2,I))];
   B = [p(2,E(1,I));p(2,E(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal tight, axis off
W1 = griddata(X,Y,ZB,U1,V1,'cubic');
[C,h] = contour(U1,V1,W1,10);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(5,1,3)
for I = 1:size(E,2)
   A = [p(1,E(1,I));p(1,E(2,I))];
   B = [p(2,E(1,I));p(2,E(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal tight, axis off
% -- Contour --------------------------
X = p(1,:); Y = p(2,:);
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1 = griddata(X,Y,ZC,U1,V1,'cubic');
[C,h] = contour(U1,V1,W1,10);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(5,1,4)
for I = 1:size(E,2)
   A = [p(1,E(1,I));p(1,E(2,I))];
   B = [p(2,E(1,I));p(2,E(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal tight, axis off
% -- Contour --------------------------
X = p(1,:); Y = p(2,:);
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1 = griddata(X,Y,ZD,U1,V1,'cubic');
[C,h] = contour(U1,V1,W1,10);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(5,1,5)
for I = 1:size(E,2)
   A = [p(1,E(1,I));p(1,E(2,I))];
   B = [p(2,E(1,I));p(2,E(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal tight, axis off
% -- Contour --------------------------
X = p(1,:); Y = p(2,:);
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1 = griddata(X,Y,ZE,U1,V1,'cubic');
[C,h] = contour(U1,V1,W1,10);