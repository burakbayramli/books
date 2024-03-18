function nrmnew = deflect_nrm(beampts,beameta,nrm,nrmpar,def)
% compute new locations for points
nbp  = length(beameta);
tmp =  size(nrm);
if length(tmp) == 3
    [npts,npp,dum] = size(nrm);
    nrm    = reshape(nrm,npts*npp,dum);
    nrmpar = reshape(nrmpar,npts*npp,2);
else
    [npts,dum]=size(tmp);
    npp = 1;
end
nrmnew = nrm;
def  = reshape(def,6,nbp)';
nrmnew = nrm;
w  = interp1(beameta,1:nbp,nrmpar(:,2));
iw = floor(w);
fr = w - iw;
for k =1:npts*npp
    iwk   = iw(k);
    iwkp1 = iwk+1;
    defpts = (1-fr(k))*def(iwk,:)   + fr(k)*def(iwkp1,:);
    rmx    = incr_rotmat(defpts(4),defpts(5),defpts(6));
    nrmnew(k,:) = nrm(k,:)*rmx';
end
if length(tmp) == 3
    nrmnew = reshape(nrmnew,npts,npp,dum);
end
