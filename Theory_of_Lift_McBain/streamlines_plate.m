alpha = pi/6; c = 1; Gamma = c * pi * sin (alpha);
W = @ (zeta) zeta + (c/4)^2 ./ zeta;
W1 = @ (zeta) W (zeta) - Gamma * log (4 * zeta / c) / pi / 2i;
W2 = @ (zeta) W1 (zeta / exp (1i * alpha));
W3 = @ (z) W2 (joukowsky_inverse (z, c));
streamlines_W (-2-2i, 2+2i, W3, -3:0.1:4 )
hold ('on')
plot ([0, 1], [0, 0], 'LineWidth', 3, 'LineStyle', '-')
