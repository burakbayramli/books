fig = figure;
alpha = pi/6; c = 1; Gamma = c * pi * sin (alpha);
W = @ (zeta) zeta + (c/4)^2 ./ zeta;
V = @ (zeta) W (zeta) - Gamma * log (4 * zeta / c) / pi / 2i;
U = @ (zeta) V (zeta / exp (1i * alpha));
conformal_streamlines (-2-2i, 2+2i, ...
		       @ (z) U (joukowsky_inverse (z, c)), ...
		       U, -3:0.1:4 )
subplot (1, 2, 1)
hold ('on')
plot ([0, 1], [0, 0], '-')
print(fig,'/tmp/listing4.7.png','-dpng')
