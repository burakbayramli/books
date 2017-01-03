function [  ] = drawVelocity( velx, vely )

rgb = zeros([size(velx) 3]);
rgb(:,:,1) = velx;
rgb(:,:,2) = vely;
rgb(:,:,3) = velx;
imagesc(rgb);

end

