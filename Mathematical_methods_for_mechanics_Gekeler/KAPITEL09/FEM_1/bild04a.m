function bild04a(p,e,t,q)
clf, hold on
X1 = p(1,:);  Y1 = p(2,:);
Z1 = zeros(1,length(X1));
trimesh(t(1:3,:)',X1,Y1,Z1,'edgecolor','g'), hold on
mesh36(p,q,'g'), hold on
axis equal, grid on
RAND = e;
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
