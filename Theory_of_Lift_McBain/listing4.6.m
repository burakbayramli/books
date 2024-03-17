fig = figure;
alpha = pi/6; c = 1;
W = @ (zeta) zeta + (c/4)^2 ./ zeta;
V = @ (zeta) W (zeta / exp (1i * alpha));
conformal_streamlines (-2-2i, 2+2i, ...
		       @ (z) V (joukowsky_inverse (z, c)), ...
		       V, -3:0.1:3 )
subplot (1, 2, 1)
hold ('on')
plot ([0, 1], [0, 0], '-')
print(fig,'/tmp/listing4.6.png','-dpng')
