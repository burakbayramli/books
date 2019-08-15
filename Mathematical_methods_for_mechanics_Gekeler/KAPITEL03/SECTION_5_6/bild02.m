function bild02(fall)
% Figure 3.17: GPV and SQP
% Call first demo1, Ex. 2 and demo2, Ex. 2
% fall = 3 for figure
clf
Parmeter = [];
plot(1.5,2,'k','markersize',6), hold on
X0 = [0;0];
Z = bsp02(X0,7,Parmeter);
W = [[Z(1,:), fliplr(Z(1,:))]; [Z(2,:), fliplr(Z(3,:))]];
fill(W(1,:),W(2,:),'y'), hold on
XZ = linspace(-1,1.5,60); YZ = linspace(-1,2,60);
[U,V] = meshgrid(XZ,YZ);
m = length(XZ); Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
% Zielfunktion ------------------------
for i = 1:m
   for k = 1:m
      U1 = [XZ(i);YZ(k)];
      Z1(k,i) = feval('bsp02',U1,1,Parmeter);
   end
end
X_OPT = [0.546;0.702];
F_OPT = feval('bsp02',X_OPT,1,Parmeter);
W1    = griddata(XZ,YZ,Z1,U,V);
%[C,h] = contour(U,V,W1,[F_OPT F_OPT],'k');
%clabel(C,h,'manual');
%contour(U,V,W1)
hold on
contour(U,V,W1,[0.2 0.6],'k'), hold on
contour(U,V,W1,[F_OPT F_OPT],'k'), hold on
plot(X_OPT(1),X_OPT(2),'*','Markersize',6), hold on
if fall == 1 | fall == 3
   % GPV-Verfahren ---
   load dateng2 PFAD
   plot(PFAD(1,:),PFAD(2,:),'ko-','markersize',6), hold on
end
if fall == 2 | fall == 3
   % SQP-Verfahren ---
   load datens2 PFAD
   plot(PFAD(1,:),PFAD(2,:),'ks-','markersize',6), hold on
end
%axis equal
text(-0.95,1.6,'SQP','fontsize',24)
text(-0.95,0,'GPM','fontsize',24)
text(-0.1,-0.3,'S','fontsize',36)
text(1.25,0.75,'f_2','fontsize',24)
%title('Beispiel 2, SQP','fontsize',18)
