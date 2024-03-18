
function [CL,CD,CL_a,resstrct,lattic,reff,geoo] = torn_one(AR,tiptaper,sweep,dihedral,twist,...
    alt,airspeed,alpha,foil1,foil2)
p  = 0;
np = 1;
if length(AR) > 1
    p = 1;
    np = length(AR);
  %  disp(['AR loop ' num2str(np)])
elseif length(tiptaper) > 1
    p = 2;
    np = length(tiptaper);
    disp(['tiptaper loop ' num2str(np)]);
elseif length(sweep) > 1
    p = 3;
    np = length(sweep);
    disp(['sweep loop ' num2str(np)])
elseif length(dihedral) > 1
    p = 4;
    np = length(dihedral);
    disp(['dihedral loop ' num2str(np)])
elseif length(twist) > 1
    p = 5;
    np = length(twist);
    disp(['twist loop ' num2str(np)])
elseif length(alpha) > 1
    p = 6;
    np = length(alpha);
    disp(['alpha loop ' num2str(np)])
end
rootchord = 1;
% 2 or 4? depends on tornado interpretation
b         = AR(1)*rootchord*(1+tiptaper(1))/4

% reference Qties
ref_pnt   = [0.25*rootchord 0 0];
CG        = ref_pnt;
%
%           nwing: 3                             # wings
% each wing:
%        symetric: [1 1 0]                     % each wing
%          startx: [-7.0592 33.0133 30.4103]   % coord. apex of wing
%          starty: [0 0 0]
%          startz: [-1.7725 2.7005 4.6555]
%           nelem: [3 2 2]                     # partitions
%               c: [15.4804 9.8206 11.7367]    % root chord
%        wingside: [3x1 double]                % if not symetric, which
%        side?
% each partition
%              ny: [nwing x nelem(max) double]   # panels y-direction
%              nx: [nwing x nelem(max) double]   # panels x-direction
%              TW: [nwing x nelem(max)x 2 double] # twist angle, inner &
%              outer edge of partition, radians
%               T: [nwing x nelem(max) double] % taper each partition
%               (chord_outer/chord_inner
%              SW: [nwing x nelem(max) double] % 1/4 chord sweep (radians)
%           dihed: [nwing x nelem(max) double] % dihedral each partition
%               b: [nwing x nelem(max) double] % y-direction extent each
%               partition
%            foil: {nwing x nelem(max)x2 cell} % airfoil each partition
%        meshtype: [nwing x nelem(max) double] % meshtype each partition
% reference q:ties
%
%       ref_point: [0 0 0]                     % obvious
%              CG: [0 0 0]                     % d:o
% TE devices:
%         flapped: [nwing x nelem(max) double] % 1 if partition has TE flap
%             fnx: [nwing x nelem(max) double] % # panels x-wise on flap
%            fsym: [nwing x nelem(max) double] % 1 sym, 0 anti-sym
%            deflection (aileron)
%              fc: [nwing x nelem(max) double] % flap chord each partition
%     flap_vector: 0
%      flap_vetor: [nwing x nelem(max) double] % misspelt ...rotation around hingeline,
%      radians
%
% LE devices:
%      flapped_LE: [3x3 double]
%          fnx_LE: [3x3 double]
%           fc_LE: [3x3 double]
%         fsym_LE: [3x3 double]
%  flap_vector_LE: [3x3 double]
%
% All-moving wings:
%         mov_all: [3x3 double]
%        fsym_all: [3x3 double]
%  flap_vector_all: [3x3 double]

% planform
nelem     = 1;
geo.nwing = 1;
geo.nelem = nelem;
geo.symetric = 1;
geo.startx = 0;
geo.starty = 0;
geo.startz = 0;
geo.c     = rootchord;
geo.T     = tiptaper(1);
geo.b     = b;
geo.SW    = sweep(1)*pi/180;
geo.dihed = dihedral(1)*pi/180;
TW        = zeros(1,1,2);
TW(1,1,1) = 0*pi/180;
TW(1,1,2) = twist(1)*pi/180;
geo.TW    = TW;

geo.foil   = cell(1,1,2);
if nargin < 9
    geo.foil{1,1,1} = '2412';
    geo.foil{1,1,2} = '2412';
else
    geo.foil{1,1,1} = foil1;
    geo.foil{1,1,2} = foil2;
end
geo.meshtype = 1;
geo.nx = 32;
geo.ny = 64;
geo.flapped     = 0;
geo.fsym        = 0;
geo.fc          = 0;
geo.fnx         = 0;
geo.flap_vector = 0;
geo.flap_vetor  = 0;
geo.ref_point   = ref_pnt;
geo.CG          = CG;
% S = b*rootchord*(1+tiptaper)/2
% AR = b^2/S; b = AR*rootchord*(1+tiptaper)/2
geo = complete_geo(geo);
% [t,xLEs,xTEs,xyzfoils,CSs] = t2xyz(geo,s.afdir);
% fusegeo = [];
% symwing = geo.symetric
% show_foil = 1
% visual_xyz(fusegeo,xLEs,xTEs,xyzfoils,CSs,symwing,show_foil)

state.AS    = airspeed;
state.betha = 0;
state.alpha = alpha(1)/180*pi;
state.ALT   = alt;
state.P     = 0;
state.Q     = 0;
state.R     = 0;
state.pgcorr = 1;
[state.rho,~,~,~] = ISAtmosphere(alt);
CL     = zeros(1,np);
CD     = CL;
CL_a   = CL;
lattictype = 0;
[lattice,ref]=fLattice_setup2(geo,state,lattictype);    %Setting up the lattice

for k = 1:np
    %    AR,tiptaper,sweep,dihedral,twist,alpha
    switch p
        case 1
            geo.b = AR(k)*rootchord*(1+tiptaper)/4;
        case 2
            geo.b = AR*rootchord*(1+tiptaper(k))/4;
            geo.T = tiptaper(k);
        case 3
            geo.SW = sweep(k)*pi/180;
        case 4
            geo.dihed = dihedral(k)*pi/180;
        case 5
            TW(1,1,2) = twist(k)*pi/180;
            geo.TW    = TW;
        case 6
            state.alpha = alpha(k)*pi/180;
        otherwise
    end
    if p ~=6 % p = 6 no need for new lattice, only alpha-sweep
        [lattice,ref]=fLattice_setup2(geo,state,lattictype);    
    end
    results = solver9([],state,geo,lattice,ref);
    results = coeff_create3(results,lattice,state,ref,geo);
   % results = trefftz5(results,state,geo,lattice,ref);
    CD(k)   = results.CD;
    CL(k)   = results.CL;
    CL_a(k) = results.CL_a;
end
if nargout > 3
    resstrct = results;
end
if nargout > 4
    lattic      = lattice;
    lattic.nx   = geo.nx;
    lattic.ny   = geo.ny;
    lattic.symm = geo.symetric;
end
if nargout > 5
    reff = ref;
end
if nargout > 6
    geoo = geo;
end
