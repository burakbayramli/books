fig = figure;
c = 1;
W = @ (zeta) zeta + (c/4)^2 ./ zeta;
conformal_streamlines (-2-2i, 2+2i, ...
		       @ (z) W (joukowsky_inverse (z, c)), ...
		       W, -2:0.1:2 )
print(fig,'/tmp/listing4.4.png','-dpng')
