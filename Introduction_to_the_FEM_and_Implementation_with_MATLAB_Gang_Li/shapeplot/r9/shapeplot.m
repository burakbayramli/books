clear;
clf;

nx=20;
ny=20;

[x,y]=meshgrid(-1:2/nx:1,-1:2/ny:1);

for j=1:nx+1
  for i=1:ny+1
    N=compShapeR9(-1,-1,1,1,x(i,j),y(i,j));
    N1(i,j)=N(1,1);    
    N2(i,j)=N(2,1);
    N3(i,j)=N(3,1);    
    N4(i,j)=N(4,1);
    N5(i,j)=N(5,1);    
    N6(i,j)=N(6,1);
    N7(i,j)=N(7,1);    
    N8(i,j)=N(8,1);
    N9(i,j)=N(9,1);
  end
end

figure(1);
surf(x,y,N1,'FaceColor',[0.5 0.5 0.5],'EdgeColor','k');
hold on
plot3([1 -1 -1 1 1], [1 1 -1 -1 1],[0 0 0 0 0],'k');
alpha(.4)
%camlight left; lighting phong
xlabel('x');
ylabel('y');
zlabel('N_1(x,y)');

hold off
print -deps N1.eps