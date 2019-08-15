function bild04(fall)
% Figure 3.19: GPV and SQP
% Call first demo2, Ex. 4 
clf
plot(5,2.5,'k','markersize',6), hold on
Parmeter = [];
X0 = [0;0];
Z = bsp04(X0,7,Parmeter);
W = [[Z(1,:),Z(3,:),Z(5,:)];[Z(2,:),Z(4,:),Z(6,:)]];
fill(W(1,:),W(2,:),'y'), hold on
plot(Z(1,:),Z(2,:),'k'), hold on
plot(Z(3,:),Z(4,:),'b'), hold on
plot(Z(5,:),Z(6,:),'r'), hold on
XA = linspace(0,5,100); Y = linspace(-1,2.5,100);
[U,V] = meshgrid(XA,Y);
m  = length(XA); Z1 = zeros(m,m); Z2 = zeros(m,m); Z3 = zeros(m,m);
% Zielfunktion ------------------------
for i = 1:m
   for k = 1:m
      U1 = [XA(i);Y(k)];
      Z1(k,i) = feval('bsp04',U1,1,Parmeter);
   end
end
X_OPT1 = [2.43199541642619; 1.26865405738502 ];
F_OPT = feval('bsp04',X_OPT1,1,Parmeter);
W1    = griddata(XA,Y,Z1,U,V);
contour(U,V,W1,[5 7 9 15 F_OPT],'k'), hold on
if fall == 1, % GPV
   %load dateng4a PFAD
   %plot(PFAD(1,:),PFAD(2,:),'ko-','markersize',6), hold on
   %load dateng4b PFAD
   %plot(PFAD(1,:),PFAD(2,:),'ks-','markersize',6), hold on
   %load dateng4c PFAD
   %plot(PFAD(1,:),PFAD(2,:),'k*-','markersize',6);
  
   load dateng4d PFAD
   plot(PFAD(1,:),PFAD(2,:),'ko-','markersize',6);
   load dateng4e PFAD
   plot(PFAD(1,:),PFAD(2,:),'ks-','markersize',6);
   load dateng4f PFAD
   plot(PFAD(1,:),PFAD(2,:),'k*-','markersize',6);
end
if fall == 2, % SQP
   load datens4a PFAD
   plot(PFAD(1,:),PFAD(2,:),'ko-','markersize',6), hold on
   load datens4b PFAD
   plot(PFAD(1,:),PFAD(2,:),'ks-','markersize',6), hold on
   load datens4c PFAD
   plot(PFAD(1,:),PFAD(2,:),'k.-','markersize',6);
end
circle(X_OPT1(1),X_OPT1(2),0.03,'g')

grid off
text(0.8,-0.3,'S','fontsize',36)
text(3.5,1.5,'f_2','fontsize',24)
%title('Beispiel 4, SQP','fontsize',24)
