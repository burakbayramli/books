function w=LPMregtrain(x,y,phifn,R)
phi = feval(phifn,x); D = size(phi,1);
if isscalar(R)
    R=R*eye(D);
end
w = (phi*phi' + R)\sum(repmat(y,D,1).*phi,2); % Gaussian Elimination