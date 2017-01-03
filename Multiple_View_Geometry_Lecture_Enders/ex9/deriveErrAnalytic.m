function [ Jac, residual ] = deriveErrAnalytic( IRef, DRef, I, xi, K )

% get shorthands (R, t)
T = se3Exp(xi);
R = T(1:3, 1:3);
t = T(1:3,4);
RKInv = R * K^-1;


% ========= warp pixels into other image, save intermediate results ===============
% these contain the x,y image coordinates of the respective
% reference-pixel, transformed & projected into the new image.
xImg = zeros(size(IRef))-10;
yImg = zeros(size(IRef))-10;

% these contain the 3d position of the transformed point
xp = NaN(size(IRef));
yp = NaN(size(IRef));
zp = NaN(size(IRef));
for x=1:size(IRef,2)
    for y=1:size(IRef,1)
        
        % point in reference image. note that the pixel-coordinates of the
        % point (1,1) are actually (0,0).
        p = [x-1;y-1;1] * DRef(y,x);
        
        % transform to image (unproject, rotate & translate)
        pTrans = RKInv * p + t;
        
        % if point is valid (depth > 0), project and save result.
        if(pTrans(3) > 0 && DRef(y,x) > 0)
            % projected point (for interpolation of intensity and gradients)
            pTransProj = K * pTrans;
            xImg(y,x) = pTransProj(1) / pTransProj(3);
            yImg(y,x) = pTransProj(2) / pTransProj(3);
            
            % warped 3d point, for calculation of Jacobian.
            xp(y,x) = pTrans(1);
            yp(y,x) = pTrans(2);
            zp(y,x) = pTrans(3);
        end
    end
end


% ========= calculate actual derivative. ===============
% 1.: calculate image derivatives, and interpolate at warped positions.
dxI = NaN(size(I));
dyI = NaN(size(I));
dyI(2:(end-1),:) = 0.5*(I(3:(end),:) - I(1:(end-2),:));
dxI(:,2:(end-1)) = 0.5*(I(:,3:(end)) - I(:,1:(end-2)));
dxInterp = K(1,1) * reshape(interp2(dxI, xImg+1, yImg+1),size(I,1) * size(I,2),1);
dyInterp = K(2,2) * reshape(interp2(dyI, xImg+1, yImg+1),size(I,1) * size(I,2),1);

% 2.: get warped 3d points (x', y', z').
xp = reshape(xp,size(I,1) * size(I,2),1);
yp = reshape(yp,size(I,1) * size(I,2),1);
zp = reshape(zp,size(I,1) * size(I,2),1);

% 3. direct implementation of kerl2012msc.pdf Eq. (4.14):
Jac = zeros(size(I,1) * size(I,2),6);
Jac(:,1) = dxInterp ./ zp;
Jac(:,2) = dyInterp ./ zp;
Jac(:,3) = - (dxInterp .* xp + dyInterp .* yp) ./ (zp .* zp);
Jac(:,4) = - (dxInterp .* xp .* yp) ./ (zp .* zp) - dyInterp .* (1 + (yp ./ zp).^2);
Jac(:,5) = + dxInterp .* (1 + (xp ./ zp).^2) + (dyInterp .* xp .* yp) ./ (zp .* zp);
Jac(:,6) = (- dxInterp .* yp + dyInterp .* xp) ./ zp;
% invert jacobian: in kerl2012msc.pdf, the difference is defined the other
% way round, see (4.6).
Jac = -Jac;

% ========= plot residual image =========
residual = reshape(IRef - interp2(I, xImg+1, yImg+1),size(I,1) * size(I,2),1);
imagesc(reshape(residual,size(I)));
colormap gray;
set(gca, 'CLim', [-1,1]);
end

