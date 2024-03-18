function ptsnew = deflect_pts(beampts,beameta,pts,ptspar,def)
% compute new locations for points
nbp  = length(beameta);
tmp =  size(pts);
%JO1701
if length(tmp) == 3
    [npts,npp,dum] = size(pts);
    pts    = reshape(pts,npts*npp,dum);
    ptspar = reshape(ptspar,npts*npp,2);
else
    [npts,dum]=size(pts);
    npp = 1;
end
def  = reshape(def,6,nbp)';
ptsnew = pts;
w  = interp1(beameta,1:nbp,ptspar(:,2));
iw = floor(w);
fr = w - iw;
for k =1:npts*npp
    iwk   = iw(k);
    iwkp1 = iwk+1;
    defpts = (1-fr(k))*def(iwk,:)     + fr(k)*def(iwkp1,:);
    beampt = (1-fr(k))*beampts(iwk,:) + fr(k)*beampts(iwkp1,:);
    rmx    = incr_rotmat(defpts(4),defpts(5),defpts(6));
    ptsnew(k,:) = beampt + (pts(k,:)-beampt)*rmx + defpts(1:3);
end
if length(tmp) == 3
    ptsnew = reshape(ptsnew,npts,npp,dum);
end
