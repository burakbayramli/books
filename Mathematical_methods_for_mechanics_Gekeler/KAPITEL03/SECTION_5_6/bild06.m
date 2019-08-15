function bild06
% Figure 3.21, GPV, various parameter of inactivation
disp(' Call first demo1, Ex. 4 ')
clf
Parmeter = [];
X0 = [0;0]; Z = bsp04(X0,7,Parmeter);
W = [[Z(1,:),Z(3,:),Z(5,:)];[Z(2,:),Z(4,:),Z(6,:)]];
fill(W(1,:),W(2,:),'y'), hold on
plot(Z(1,:),Z(2,:),'k'), hold on
plot(Z(3,:),Z(4,:),'b'), hold on
plot(Z(5,:),Z(6,:),'r'), hold on
XA = linspace(0,4.5,60); Y = linspace(-1,2.5,60);
[U,V] = meshgrid(XA,Y);
m = length(XA); Z1 = zeros(m,m); Z2 = Z1; Z3 = Z1;
% Zielfunktion ------------------------
for i = 1:m
   for k = 1:m
      U1 = [XA(i);Y(k)];
      Z1(k,i) = feval('bsp04',U1,1,Parmeter);
   end
end
X_OPT1 = [2.43;1.27];
F_OPT = feval('bsp04',X_OPT1,1,Parmeter);
W1    = griddata(XA,Y,Z1,U,V);
%[C,h] = contour(U,V,W1,[F_OPT F_OPT],'k');
%clabel(C,h,'manual');
contour(U,V,W1,[8 9 13 F_OPT],'k'), hold on
plot(X_OPT1(1),X_OPT1(2),'.','Markersize',6), hold on
% -- GPV -------------------------------
load dateng4d PFAD
plot(PFAD(1,:),PFAD(2,:),'k*-','markersize',6), hold on
load dateng4e PFAD
plot(PFAD(1,:),PFAD(2,:),'ko-','markersize',6), hold on
load dateng4f PFAD
plot(PFAD(1,:),PFAD(2,:),'ks-','markersize',6), hold on

grid on
text(0.8,-0.3,'S','fontsize',36)
text(3.5,1.5,'f','fontsize',18)
%title('Beispiel 4, SQP','fontsize',18)
