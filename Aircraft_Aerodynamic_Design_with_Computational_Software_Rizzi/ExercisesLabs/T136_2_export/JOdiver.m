% move on with divergence
% 1 - run tornado
clear all
close all
format compact
clc
%
load aircraft/test_geo_1;
load aircraft/A320w;
load aircraft/UAVgeo
geo
%        nwing: 3
geo.nwing = 1
%           wingNames: {3x1 cell}
geo.wingNames = geo.wingNames{1};
%               nelem: [8 3 3]
geo.nelem = geo.nelem(1);
%            symetric: [1 1 0]
geo.symetric = geo.symetric(1);
%              startx: [9.1968 14.9158 13.3532]
geo.startx = geo.startx(1);
%              starty: [0 0 0]
geo.starty = 0;
%              startz: [2.5139 5.3416 3]
geo.startz = geo.startz(1);
%                   c: [3x9 double]
geo.c  = geo.c(1,:);
%                   b: [3x8 double]
geo.b = geo.b(1,:);
%               dihed: [3x8 double]
geo.dihed = geo.dihed(1,:);
%                   T: [3x8 double]
geo.T = geo.T(1,:);
%                SWle: [3x8 double]
geo.SWle = geo.SWle(1,:);
%                SW25: [3x8 double]
geo.SW25 = geo.SW25(1,:);
%                SWte: [3x8 double]
geo.SWte = geo.SWte(1,:);
%                  SW: [3x8 double]
geo.SW = geo.SW(1,:);
%                  TW: [3x8x2 double]
geo.TW = geo.TW(1,:,:);
%                foil: {3x8x2 cell}
geo.foil = geo.foil(1,:,:);
%                  ny: [3x8 double]
geo.ny = geo.ny(1,:);
%                  nx: [3x8 double]
geo.nx = geo.nx(1,:);
%              csname: {3x8x3 cell}
geo.csname = geo.csname(1,:,:);
%              cslist: {[1x1 struct]  [1x1 struct]  [1x1 struct]  [1x1 struct]}
geo.cslist=geo.cslist(1:2);
%             flapped: [3x8 double]
geo.flapped = geo.flapped(1,:);
%                fsym: [3x8 double]
geo.fsym = geo.fsym(1,:);
%                 fnx: [3x8 double]
geo.fnx  = geo.fnx(1,:);
%                  fc: [3x8 double]
geo.fc = geo.fc(1,:);
%         flap_vector: [3x8 double]
geo.flap_vector = geo.flap_vector(1,:);
%     flap_vector_max: [3x8 double]
geo.flap_vector_max = geo.flap_vector_max(1,:);
%     flap_vector_min: [3x8 double]
geo.flap_vector_min = geo.flap_vector_min(1,:);
%              flapID: [3x8 double]
geo.flapID = geo.flapID(1,:);
%           ref_point: [10.0765 0 0]
%                  CG: [10.0765 0 0]
%     ref_point_CPACS: [0 0 0]
%            wingside: [3x1 double]
geo.wingside = geo.wingside(1,:);
%          flapped_LE: [3x8 double]
geo.flapped_LE = geo.flapped_LE(1,:);
%              fnx_LE: [3x8 double]
geo.fnx_LE = geo.fnx_LE(1,:);
%               fc_LE: [3x8 double]
geo.fc_LE = geo.fc_LE(1,:);
%             fsym_LE: [3x8 double]
geo.fsym_LE = geo.fsym_LE(1,:);
%      flap_vector_LE: [3x8 double]
geo.flap_vector_LE = geo.flap_vector_LE(1,:);
%             mov_all: [3x8 double]
geo.mov_all = geo.mov_all(1,:);
%            fsym_all: [3x8 double]
geo.fsym_all = geo.fsym_all(1,:);
%     flap_vector_all: [3x8 double]
geo.flap_vector_all = geo.flap_vector_all(1,:);
%           hingepnt1: [4-D double]
geo.hingepnt1 = geo.hingepnt1(1,:,:,:);
%           hingepnt2: [4-D double]
geo.hingepnt2 = geo.hingepnt1(1,:,:,:);
%            winglist: {[]  []  []}
geo.winglist = geo.winglist(1);
pause
% has no foil, so
%geo.foil = cell(1,5,2);
%for i = 1:5
%    for j = 1:2
%        geo.foil{1,i,j}='naca0012.dat';
%    end
%end
load aircraft/structure/test_structure_1;
structure.skin_thick = 0.002
structure


% loop over airspeed
as = linspace(100,110,4)
iff = 16
for avel = as
    iff = iff+1;
%     figure(25)
%     clf
%     figure(26)
%     clf
    figure(iff)
    clf
    % better resolution
     geo.nx = 2*geo.nx;
     geo.ny = 2*geo.ny;
     geo.fnx = 2*geo.fnx;
    structure.nx = 30;
    structure.spars = [0.2 0.7];
    state.AS     =avel;
    state.P      =0;
    state.Q      =0;
    state.R      =0;
    state.alpha  = 4*pi/180;
    state.betha  = 0;
    state.pgcorr = 0;
    state.rho    = 1.273;
    
    'flat2'
    [lattice,ref]=fLattice_setup2(geo,state,0);
    txt = '0';
    figure(iff)
    g=fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','w');
    set(g,'LineWidth',2);
    axis equal
    hold on
    title([num2str(avel), ' :  ', txt])
    results = [];
    % cfd
    'solver9'
    [results]=solver9(results,state,geo,lattice,ref);
    'coeff3'
    [results]=coeff_create3(results,lattice,state,ref,geo);
    pause
    wingno = 1;
    % struct
    'gstructmesh'
    mesh = generate_struct_mesh(geo,lattice,structure,wingno);
    latticenew = lattice;
    def = zeros(360,1);
    for kk = 1:5
        txt = [txt ' ' num2str(kk)];
        % struct
        'femsolver'
        FEMresult=FEMsolver(structure.nx,geo,latticenew,state,ref,structure,mesh,results,wingno);
        FEMresultplot2(FEMresult,mesh);
        % deform
        defnew = FEMresult.def;
        dd(kk) = norm(defnew-def);
        def    = defnew;
        'ftlatdef'
        [latticenew,meshnew] = ftlatticedeform...
            (geo,lattice,mesh,FEMresult,wingno);
        figure(iff)
        g=fill3(latticenew.XYZ(:,:,1)',latticenew.XYZ(:,:,2)',latticenew.XYZ(:,:,3)','w');
        set(g,'LineWidth',2);
        title([num2str(avel), ' :  ', txt]);
        axis equal
        hold on
        % cfd
        'solver9'
        [results]=solver9(results,state,geo,latticenew,ref);
        'coeff3'
        [results]=coeff_create3(results,latticenew,state,ref,geo);
    end
    disp([avel dd])
end