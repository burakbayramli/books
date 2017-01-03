%do_quiver
[x,y] = meshgrid(-2:.5:2,-1:.25:1);
z = x.*exp(-x.^2 - y.^2); 
[px,py] = gradient(z,.5,.25);
contour(x,y,z), hold on, quiver(x,y,px,py)
axis image %the same as AXIS EQUAL except that 
           %the plot box fits tightly around the data
