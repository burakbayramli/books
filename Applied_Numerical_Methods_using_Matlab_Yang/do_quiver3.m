%do_quiver3
[x,y] = meshgrid(-2:.5:2,-1:.25:1);
z = x.*exp(-x.^2 - y.^2);
surf(x,y,z), hold on
[u,v,w] = surfnorm(x,y,z);
quiver3(x,y,z,u,v,w); 
