function XYZcell = var2xyz(startxyz,b,c,dihed,T,SW,TW,symetric,wingside)
% create polygons XYZ{1:ncurve}(1:np,1:3);
% input:
%-------------------------
% startxyz(1:3) apex
% b(k)       y-coord of kink
% c          root chord
% dihed(k)   dihedral of wing segment
% T(k)       taper of kink k
% SW(k)      quarterchord sweep of segment k
% TW(k)      twist angle segment k
% symetric   0: non-symm 1: make a mirror copy
% wingside   1: starb.  -1: port
%-------------------------
% calls:
% --
%-------------------------
% JOP 100530
nelem = length(dihed);
xLE = ones(nelem+1,1)*startxyz;
xTE = xLE;
xQC = xLE;
fac = c;
ii  = TW(1,1);
xTE(1,:)=xLE(1,:)+fac*[cos(ii),0,-sin(ii)];
xQC(1,:)=0.75*xLE(1,:)+0.25*xTE(1,:);
for k = 1:nelem
    fi  = SW(k);
    the = dihed(k)
    s   = b(k);
    xQC(k+1,:)= xQC(k,:)+s*[tan(fi),cos(the),sin(the)];
    fac       = fac*T(k);
    ii        = TW(k,2);
    xLE(k+1,:)= xQC(k+1,:) - 0.25*fac*[cos(ii),0,-sin(ii)];
    xTE(k+1,:)= xLE(k+1,:) + 4*(xQC(k+1,:)-xLE(k+1,:));
end
XYZ = [xLE;flipud(xTE)];
if symetric % create a copy
    XYZc      = XYZ;
    XYZc(:,2) = -XYZc(:,2);
    XYZcell{1}=XYZ;
    XYZcell{2}=XYZc;
else
    XYZcell{1} = XYZ;
end
