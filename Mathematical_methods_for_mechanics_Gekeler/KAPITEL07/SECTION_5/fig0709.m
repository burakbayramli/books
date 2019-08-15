function fig0709
% plots spatial framework

load daten6 Z p e LAGER LASTEN PARMETER
FAKTOR = 2;
clf
plot3(p(1,:),p(2,:),p(3,:),'.','markersize',8,'color','b'), hold on
for I = 1:size(e,2)
   plot3(p(1,e(1:2,I)),p(2,e(1:2,I)),p(3,e(1:2,I)),'b','linewidth',2);
   hold on
end
Q = FAKTOR*Z(1:3,:) + p;
plot3(Q(1,:),Q(2,:),Q(3,:),'.','markersize',8,'color','r'), hold on
for I = 1:size(e,2)
   plot3(Q(1,e(1:2,I)),Q(2,e(1:2,I)),Q(3,e(1:2,I)),'r--','linewidth',2);
   hold on
end

triflag = 0;
if triflag == 1
   for I = 1:size(TRI,2)
      fill3(p(1,TRI(:,I)),p(2,TRI(:,I)),p(3,TRI(:,I)),'w'), hold on
   end
end
plot3(-1,-1,0,'k.','Markersize',6), hold on
plot3(11,-1,0,'k.','Markersize',6), hold on
%view([0 -90]);
xlabel('x')
ylabel('y')
zlabel('z')

axis equal
grid on
