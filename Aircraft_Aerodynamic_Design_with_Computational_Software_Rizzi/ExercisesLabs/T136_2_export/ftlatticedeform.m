function [latticenew,meshnew] = ftlatticedeform...
    (geo,lattice,mesh,FEMresult,wingno)
% Vortex points etc.
VORTEX = lattice.VORTEX;
XYZ    = lattice.XYZ;
COLLOC = lattice.COLLOC;
N      = lattice.N;
Ppar   = lattice.Ppar;
Cpar   = lattice.Cpar;
Vorpar = lattice.Vorpar;
% beam points etc.
GP_SB  = mesh.GP_SB;
beameta_SB = mesh.beameta_SB;
nbpt = size(GP_SB,1);
% beam point deflections
def = FEMresult.def;
def = [zeros(6,1);def];
[nvrt npv s3] = size(lattice.VORTEX);
% figure(99)
% hold on
% for k= 1:nvrt
%     plot3(XYZ(k,:,1),XYZ(k,:,2),XYZ(k,:,3),'.-k')
%     hold on
% end
%
nx = geo.nx(wingno,:)+geo.fnx(wingno,:);
ny = geo.ny(wingno,:);
npan = length(nx);           %number of wings and panels

cny = ny.*(geo.symetric(wingno)+1); %corrected number of ypanels
vrtxnew = VORTEX;
beampts = GP_SB;
beameta_t = beameta_SB;
def2 = def;
nd   = length(def);
deff = reshape(def,6,nd/6)';
nSB = size(GP_SB,1);
if geo.symetric(wingno)
    beampts   = [flipud(mesh.GP_P(2:end,:));beampts];
    beameta_t = [fliplr(mesh.beameta_P(2:end)),beameta_t];
    deff = [flipud(deff(nSB+1:end,:));deff(1:nSB,:)]';
    def2 = deff(:);
end
% deflects all VLM lattice data, should only be for wing wingno!
vrtxnew = deflect_pts(beampts,beameta_t,VORTEX(:,2:npv-1,:),(Vorpar),def2);
xyznew  = deflect_pts(beampts,beameta_t,XYZ,(Ppar),def2);
collnew = deflect_pts(beampts,beameta_t,COLLOC,(Cpar),def2);
nnew    = deflect_nrm(beampts,beameta_t,N,(Cpar),def2);

beamptnew = beampts' + deff(1:3,:);
beamptnew = beamptnew';
%
% figure(99)
% hold on
% for k= 1:nvrt
%     plot3(xyznew(k,:,1),xyznew(k,:,2),xyznew(k,:,3),'.-r')
%     hold on
% end
latticenew        = lattice;
latticenew.VORTEX(:,2:npv-1,:) = vrtxnew;
latticenew.XYZ    = xyznew;
latticenew.COLLOC = collnew;
latticenew.N      = nnew;
% beam points etc.

meshnew    = mesh;
mesh.GP_SB = beamptnew;
if geo.symetric(wingno)
    mesh.GP_SB = beamptnew(end-nSB+1:end,:);
    mesh.GP_P(2:nbpt,:) = flipud(beamptnew(1:nbpt-1,:));
end










