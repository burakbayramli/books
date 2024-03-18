
% 1 - run tornado
clear all
close all
format compact
clc
% geo =
%
%             fnx: [0 1 1 1 0]
%              ny: [3 4 8 4 1]
%              nx: [5 4 4 4 5]
%            fsym: [0 1 1 0 0]
%              fc: [0 0.2500 0.2500 0.2500 0]
%         flapped: [0 1 1 1 0]
%              TW: [1x5x2 double]
%            foil: {1x5x2 cell}
%               T: [1 0.6172 0.6701 0.8000 0.9185]
%              SW: [0 0.4054 0.4747 0.4747 0.4747]
%               c: 6.7380
%           dihed: [0.0698 0.0698 0.0698 0.0698 0.0698]
%               b: [1.8680 4.9070 8.5270 3.7210 0.8140]
%        symetric: 1
%          startx: 0
%          starty: 0
%          startz: 0
%           nwing: 1
%           nelem: 5
%     flap_vector: [0 0 0 0 0]
%       ref_point: [0 0 0]
%              CG: [0 0 0]
rtube = 1
c = 0.8
b = 0.8
l = 3
alpha = 2*pi/180;
nsect = 8;
nwing = 2
geo.nwing = nwing
geo.symetric = [1 1]
geo.nelem  = [1 nsect];
nel = max(geo.nelem);
geo.ny = zeros(2,nsect);
geo.ny(1,1) = 5;
geo.ny(2,:) = ones(1,nsect)*4;
geo.nx = ones(2,nel)*5;
geo.nx(2,:) =15;
geo.SW = zeros(nwing,nel);
geo.c  = [c 2*l+c];
geo.b      = zeros(nwing,nel);
geo.b(1,:) = 0.5*ones(1,nel);
geo.b(2,:) = 2*pi*rtube/nsect;
ddihed = 2*pi/nsect;
geo.startx     = [0 -l-c/2];
geo.starty     = [rtube+0.04 0];
geo.startz     = [0  -rtube];
geo.TW         = zeros(nwing,nel,2);
%geo.TW(2,:,:)  = alpha;
geo.TW(1,:,:)  = alpha
geo.dihed      = zeros(nwing,nel);
geo.dihed(2,:) = ddihed*(0:nsect-1)+ddihed/2;
geo.T          = ones(nwing,nsect);
geo.fsym       = zeros(nwing,nsect);
geo.flapped    = zeros(nwing,nsect);
geo.fc         = zeros(nwing,nsect);
geo.flapvector = zeros(nwing,nel);
geo.foil = cell(nwing,nsect,2);
geo.fnx = zeros(nwing,nsect);

state.AS     =1;
state.P      =0;
state.Q      =0;
state.R      =0;
state.alpha  = 0*pi/180;
state.betha  = 0;
state.pgcorr = 0;
state.rho    = 1000;
for i =1:nwing
    for j = 1:nsect
        for k = 1:2
            geo.foil{i,j,k}='0012';
        end
    end
end

geo.ref_point = [0 0 0];
geo.CG       = [0 0 0];
[lattice,ref]=fLattice_setup2(geo,state,0);
figure(1)
g=fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','w');
set(g,'LineWidth',2);
axis equal
hold on
'solver9'
results = [];
[results]=solver9(results,state,geo,lattice,ref);
'coeff3'
%[results]=coeff_create3(results,lattice,state,ref,geo);
