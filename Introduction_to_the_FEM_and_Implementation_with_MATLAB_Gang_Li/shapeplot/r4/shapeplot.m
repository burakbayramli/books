clear;
clf;

nx=20;
ny=20;

[x,y]=meshgrid(-1:2/nx:1,-1:2/ny:1);

for j=1:nx+1
  for i=1:ny+1
    N=compShapeR4(-1,-1,1,1,x(i,j),y(i,j));
    N1(i,j)=N(1,1);    
    N2(i,j)=N(2,1);
    N3(i,j)=N(3,1);    
    N4(i,j)=N(4,1);
  end
end

figure(1);
surf(x,y,N2,'FaceColor',[0.5 0.5 0.5],'EdgeColor','k');
hold on
plot3([1 -1 -1 1 1], [1 1 -1 -1 1],[0 0 0 0 0],'k');
alpha(.4)
%camlight left; lighting phong
xlabel('x');
ylabel('y');
zlabel('N_2(x,y)');
set(gca,'fontsize',16);
hold off
%print -depsc N2.eps
print -djpeg -r300 N2.jpg