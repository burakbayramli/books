fig = figure;
AR = 5.0;
taper = 1.0;
sweep = pi/4;
nchord = 2;
nspan = 8;
[x, y, z] = meshwing (AR, nchord, nspan, sweep, ...
		      0, taper, 0, @ (z) 0*z);
meshwing_plot (x, y, z);
[r, r1, r2] = meshwing_vlm (x, y, z);
ihat = repmat ([1; 0; 0], 1, nchord * nspan);
q = vortex_horseshoe (r(:,:), r1(:,:), r2(:,:), ihat);
v = squeeze (q(2,:,:));
Gamma = v \ -ones (nchord * nspan, 1);
CLa = 2 * AR * sum (Gamma) / nspan
print(fig,'/tmp/bertin2.png','-dpng')
