fig = figure;
AR = 6.0;
taper = 0.6;
sweep = atan ((1-taper)/(1+taper)/AR + tan (pi/4));
n = 2 * 10;
[x, y, z] = meshwing (AR, 1, n, sweep, 0, taper, ...
		      0, @ (z) 0*z);
meshwing_plot (x, y, z);
[r, r1, r2] = meshwing_vlm (x, y, z);
ihat = repmat ([1; 0; 0], 1, n);
qrr = vortex_horseshoe (r(:,n/2+1:n), ...
			r1(:,n/2+1:n), ...
			r2(:,n/2+1:n), ...
			ihat(:,1:n/2) );
vrr = squeeze (qrr(2,:,:));
qrl = vortex_horseshoe (r(:,n/2+1:n), ...
			fliplr (r1(:,1:n/2)), ...
			fliplr (r2(:,1:n/2)), ...
			ihat(:,1:n/2) );
vrl = squeeze (qrl(2,:,:));
Gamma = (vrr + vrl) \ -ones (n/2, 1);
disp (reshape (2 * flipud (Gamma), n/4, 2))
CLa = 4 * AR * sum (Gamma) / n
print(fig,'/tmp/cambell.png','-dpng')
