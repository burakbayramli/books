fig = figure;
alpha = pi/6; c = 1; Gamma = c * pi * sin (alpha);
W0 = @ (z) exp (-1i * alpha) * z;
W = @ (z) W0 (z) - Gamma * log (z - c/4) / pi / 2i;
streamlines_W (-2-2i, 2+2i, W, -3:0.1:4)
hold ('on')
plot ([0, 1], [0, 0], '+-', 'LineWidth', 3, ...
      1/4, 0, 'o', 3/4, 0, 'x')
axis ('image')
print(fig,'/tmp/lvm1.png','-dpng')
