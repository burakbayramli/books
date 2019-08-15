function bild
clear
[X,Y] = meshgrid([-2:0.15:2],[-4:0.05:4]);
Z = 0.5*sin(2*Y);
%mesh(X,Y,Z);
h = surf(X,Y,Z,'EdgeColor','none');
colormap gray
set(gca,'DataAspectRatio',[1,1,1])
set(h,'ambientstrength',1)
light('Position',[0,4,4])
xlabel('X Axis')
ylabel('Y Axis')
axis equal tight
%axis([-2 2 -4 4 -2 2])
%get(h)
