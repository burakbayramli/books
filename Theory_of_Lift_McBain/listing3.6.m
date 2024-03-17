fig = figure;
r = exp (-1i * pi / 6);
w = @ (z) (abs (z) > 1) .* (1 - z .^ -2);
direction (-2-2i, 2+2i, @ (z) r * w (r * z))
print(fig,'/tmp/listing3.6.png','-dpng')
