function [ err ] = calcErr( IRef, DRef, I, xi, K )

% get shorthands (R, t)
T = se3Exp(xi);
R = T(1:3, 1:3);
t = T(1:3,4);

RKInv = R * K^-1;

% these contain the x,y image coordinates of the respective
% reference-pixel, transformed & projected into the new image.
% set to -10 initially, as this will give NaN on interpolation later.
xImg = zeros(size(IRef))-10;
yImg = zeros(size(IRef))-10;

% for all pixels
for x=1:size(IRef,2)
    for y=1:size(IRef,1)
        % point in reference image. note that the pixel-coordinates of the
        % point (1,1) are actually (0,0).
        p = [x-1;y-1;1] * DRef(y,x);
        
        % transform to image (unproject, rotate & translate)
        pTrans = K * (RKInv * p + t);
        
        % if point is valid (depth > 0), project and save result.
        if(pTrans(3) > 0 && DRef(y,x) > 0)
            xImg(y,x) = pTrans(1) / pTrans(3) + 1;
            yImg(y,x) = pTrans(2) / pTrans(3) + 1;
        end
    end
end
    
% calculate actual residual (as matrix).
err = IRef - interp2(I, xImg, yImg);

% plot residual image
imagesc(err);
colormap gray;
set(gca, 'CLim', [-1,1]);

err = reshape(err,size(I,1) * size(I,2),1);

end

