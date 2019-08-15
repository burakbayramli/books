function bild00(p,e,t)
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
if ~isempty(t)
   trimesh(t(1:3,:).',X,Y,Z1,'edgecolor','k'), hold on
end
plot(X,Y,'.'), hold on
axis equal, grid on
for I = 1:size(e,2)
   A = p(1,e(1:2,I));
   B = p(2,e(1:2,I));
   plot(A,B,'r','linewidth',1), hold on
   plot(A,B,'r.','markersize',6), hold on
end
% for example 1:
flag = 0;
if flag == 1
   x = 0.25/7; phi = asin(3/(3+x)); MP = [3-x;3]; RAD = 3+x;
   TT = linspace(0,-phi,40);
   XR = MP(1) + RAD*cos(TT); YR = MP(2) + RAD*sin(TT); 
   plot(XR,YR,'r'), hold on
   XR = 3+x - RAD*cos(TT); YR = MP(2) + RAD*sin(TT); 
   plot(XR,YR,'b'), hold on

end

%axis off
