fig = figure;
powmag = @ (z, k) abs (z) .^ k;
powarg = @ (z, k) k * mod (angle (z), 2 * pi);
pow = @ (z, k) powmag (z, k) .* exp (1i * powarg (z, k));
conformal_streamlines (-2-2i, 2+2i, ...
		       @ (Z) Z, @ (Z) pow (Z, 2/3), ...
		       [1e-6, 0.5:0.5:2])
print(fig,'/tmp/listing4.3.png','-dpng')
