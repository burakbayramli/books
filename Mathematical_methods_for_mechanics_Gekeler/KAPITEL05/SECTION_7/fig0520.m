function fig0520
% Figure 5.19 Niveau lines

disp(' Call first DEMO1-1 ')
clf
load daten1 WEG1 HOEHEN1 WEG2 HOEHEN2
[M,N] = size(WEG1);
M     = M/2;
for I = 1:M
   plot3(WEG1(2*I-1,:),WEG1(2*I,:),HOEHEN1(I,:),'r','linewidth',2), hold on
   plot3(WEG1(2*I-1,1),WEG1(2*I,1),HOEHEN1(I,1),'r.','Markersize',6), hold on
   plot3(WEG1(2*I-1,N),WEG1(2*I,N),HOEHEN1(I,N),'r*','Markersize',6)
end
[M,N] = size(WEG2);
M     = M/2;
for I = 1:M
   plot3(WEG2(2*I-1,:),WEG2(2*I,:),HOEHEN2(I,:),'b','linewidth',2), hold on
   plot3(WEG2(2*I-1,1),WEG2(2*I,1),HOEHEN2(I,1),'.','Markersize',6), hold on
   plot3(WEG2(2*I-1,N),WEG2(2*I,N),HOEHEN2(I,N),'*','Markersize',6)
end
plot3(1,0,1,'.','markersize',12), hold on
plot3(-1,0,1,'.','markersize',12), hold on
axis equal
grid on
xlabel('x','fontsize',22);
ylabel('y','fontsize',22);
zlabel('z','fontsize',22);
