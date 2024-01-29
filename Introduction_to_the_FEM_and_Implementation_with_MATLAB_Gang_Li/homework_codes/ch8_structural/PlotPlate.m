% for-loop: plot the undeformed plate
% for e=1:nElements
%   for j=2:3
%     n1=nodes(elements(e,j),2:4);
%     n2=nodes(elements(e,j+1),2:4);
%     plot3([n1(1) n2(1)],[n1(2) n2(2)],[n1(3) n2(3)],'k--');
%   end 
%   n1=nodes(elements(e,2),2:4);
%   plot3([n1(1) n2(1)],[n1(2) n2(2)],[n1(3) n2(3)],'k--');
% end
%plot the deformed plate
p=patch('Vertices',nodes(:,2:4),'Faces',elements(:,2:4));
set(p,'facecolor',[0.7 0.7 0.7],'edgecolor','black');
%axis([-15 15 0 20 -1 0.2]);
xlabel('X'); ylabel('Y'); zlabel('Z');
view(110,20);

