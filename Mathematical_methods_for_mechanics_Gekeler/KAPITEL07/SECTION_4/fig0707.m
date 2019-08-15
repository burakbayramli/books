function bld070403;
%Zeichnet raeumliches Stabwerk Beispiel 3
set(gcf,'renderer','zbuffer')
load daten3 Z p e LAGER LASTEN PARMETER
clf
FAKTOR = 50;
plot3(p(1,:),p(2,:),p(3,:),'.','markersize',8,'color','b'), hold on
for I = 1:size(e,2)
   plot3(p(1,e(:,I)),p(2,e(:,I)),p(3,e(:,I)),'k','linewidth',1.5), hold on
end
p = p + FAKTOR*Z';
for I = 1:size(e,2)
   plot3(p(1,e(:,I)),p(2,e(:,I)),p(3,e(:,I)),'r--','linewidth',1.5), hold on
end

%view([0 -90]);
axis equal
grid on
