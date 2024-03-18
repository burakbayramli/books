lattice_new = lattice;

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
%GP_P   = mesh.GP_P;
beameta_SB = mesh.beameta_SB;
%beameta_P  = mesh.beameta_P;
% beam point deflections
def   = FEMresult.def;
def = [zeros(6,1);def];
[s1 npv s3] = size(lattice.VORTEX)
figure(99)
hold on
for k= 1:s1
    plot3(XYZ(k,:,1),XYZ(k,:,2),XYZ(k,:,3),'.-k')
    hold on
end
%
nx = geo.nx(wingno,:)+geo.fnx(wingno,:);
ny = geo.ny(wingno,:);
npan = length(nx);           %number of wings and panels

cny = ny.*(geo.symetric(wingno)+1); %corrected number of ypanels
vrtxnew = VORTEX;
beampts = GP_SB;
beameta_t = beameta_SB;
nd = length(def);
nSB = size(GP_SB,1);
%deff = reshape(def,6,nd/6)';
%deff = [flipud(deff(nSB+1:end,:));deff(1:nSB,:)]';
def2 = def;
deflect_pts(beampts,beameta_t,VORTEX(:,2:npv-1,:),(Vorpar),def2);
xyznew  = deflect_pts(beampts,beameta_t,XYZ,(Ppar),def2);
collnew = deflect_pts(beampts,beameta_t,COLLOC,(Cpar),def2);
nnew    = deflect_nrm(beampts,beameta_t,N,(Cpar),def2);
% 
figure(99)
hold on
for k= 1:s1
    plot3(xyznew(k,:,1),xyznew(k,:,2),xyznew(k,:,3),'.-r')
    hold on
end











