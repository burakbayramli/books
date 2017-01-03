n=256; d=1;
coeff = complex(randn(n,n), randn(n,n));
freq = [0 1:n/2 ((n/2)-1):-1:1];
filt = 1./((freq'.^2)*ones(1,n)+ones(n,1)*freq.^2); filt(1,1)=0;
landscape = real(ifft2(coeff.*(filt.^d)));
c = [0 .5 1; .5 .5 .5; zeros(17,1) (1:-.05:.2)' zeros(17,1);...
    .5 .5 .5; ones(5,3)];
surf(max(landscape,0),'EdgeColor','none','AmbientStrength',.5, ...
    'FaceLighting','Phong' ), 
set(gca, 'AmbientLightColor', [.3 .3 1]); colormap(c); 
axis([0 257 0 257 0 2*max(landscape(:))]), axis off 
light('Position',[1 0 .2], 'Style', 'infinite', 'Color', [1 .75 0]);
