function bild03(fall)
% Figure 3.18: GPV and SQP
% Call first demo1, Ex. 3 and demo2, Ex. 3
clf
plot(5,2.5,'k','markersize',6), hold on
Parmeter = []; X0 = [0;0];
Z = bsp03(X0,7,Parmeter);
W = [[Z(1,:),Z(3,:),Z(5,:)];[Z(2,:),Z(4,:),Z(6,:)]];
fill(W(1,:),W(2,:),'y'), hold on
plot(Z(1,:),Z(2,:),'k'), hold on
plot(Z(3,:),Z(4,:),'b'), hold on
plot(Z(5,:),Z(6,:),'r'), hold on
X = linspace(1,5,40); Y = linspace(-2,2.5,40);
[U,V] = meshgrid(X,Y);
m = length(X); Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
% Zielfunktion ------------------------
for i = 1:m
   for k = 1:m
      U1 = [X(i);Y(k)];
      Z1(k,i) = feval('bsp03',U1,1,Parmeter);
   end
end
X_OPT = [3.45;0.19];
F_OPT = feval('bsp03',X_OPT,1,Parmeter);
W1    = griddata(X,Y,Z1,U,V);
%[C,h] = contour(U,V,W1,[F_OPT F_OPT],'k');
%clabel(C,h,'manual');
hold on
%contour(U,V,W1,[3 3],'k');
hold on
contour(U,V,W1,[4 7 F_OPT],'k'), hold on
%contour(U,V,W1,[F_OPT F_OPT],'k'), hold on
plot(X_OPT(1),X_OPT(2),'.','Markersize',6), hold on
if fall == 1 | fall == 3
   % -- GPV ---
   load dateng3 PFAD
   plot(PFAD(1,:),PFAD(2,:),'ko-','markersize',12), hold on
end
if fall == 2 | fall == 3
   % -- SQP ---
   load datens3a PFAD
   plot(PFAD(1,:),PFAD(2,:),'k*-','markersize',12), hold on
   load datens3b PFAD
   plot(PFAD(1,:),PFAD(2,:),'ks-','markersize',12),
end
grid off
%axis equal
text(-0.95,2.2,'SQP','fontsize',24)
text(-0.95,-2.2,'SQP','fontsize',24)
text(-0.95,-0.2,'GPM','fontsize',24)
text(1,-0.7,'S','fontsize',36)
text(3.3,1.3,'f_1','fontsize',24)
