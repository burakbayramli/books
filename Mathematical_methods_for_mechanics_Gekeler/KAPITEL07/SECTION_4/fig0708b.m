function bld070404b;
% -- Zeichnet raeumliches Stabwerk, Beispiel 4
set(gcf,'renderer','zbuffer')
load daten4 Z p e LAGER LASTEN PARMETER TRI
clf
FAKTOR = 1e3;
plot3(p(1,:),p(2,:),p(3,:),'.','markersize',8,'color','b'), hold on
for I = 1:size(e,2)
   plot3(p(1,e(:,I)),p(2,e(:,I)),p(3,e(:,I)),'k','linewidth',1),hold on
end
p = p + FAKTOR*Z';
for I = 1:size(e,2)
   plot3(p(1,e(:,I)),p(2,e(:,I)),p(3,e(:,I)),'r--','linewidth',1),hold on
end


%for I = 1:size(TRI,2)
%   fill3(p(1,TRI(:,I)),p(2,TRI(:,I)),p(3,TRI(:,I)),'y');
%   hold on
%end
%view([0 -90]);
axis equal
grid off
%get(h)
