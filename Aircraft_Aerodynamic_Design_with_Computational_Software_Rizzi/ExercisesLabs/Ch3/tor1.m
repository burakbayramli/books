function [coeffse,resstrct,lattic,reff,geoo] = ...
  tor1(AR,tap,swe,dih,twi,...
    alt,airspeed,al,foil1,foil2,resimn,resimx)
rootchord = 1;
b   = AR*rootchord*(1+tap)/4;
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
  geo.T     = tap;
  geo.b     = b;
  geo.SW    = swe;
  geo.dihed = dih;
  TW        = zeros(1,1,2);
  TW(1,1,1) = 0;
  TW(1,1,2) = twi;
  geo.TW    = TW;
  geo.foil   = cell(1,1,2);
  if nargin < 9
    geo.foil{1,1,1} = 'naca0012';
    geo.foil{1,1,2} = 'naca0012';
  else
    geo.foil{1,1,1} = foil1;
    geo.foil{1,1,2} = foil2;
  end
  geo.meshtype = 1;
  
  %=================  nx and ny
  geo.nx = 2;
  geo.ny = round(AR);
  %=================
  
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

  state.AS    = airspeed;
  state.betha = 0;
  state.alpha = al;
  state.ALT   = alt;
  state.P     = 0;
  state.Q     = 0;
  state.R     = 0;
  state.pgcorr = 1;
  [state.rho,~,~,~] = ISAtmosphere(alt);
  lattictype = 0;
  torfoil    = 0;
 % resimn     = 2;
 % resimx     = 4;
  [coeffse,results] = VLM_e(resimn,resimx,geo,state,torfoil,lattictype,[]);
   % results = trefftz5(results,state,geo,lattice,ref);
end
if nargout > 1
  resstrct = results;
end
if nargout > 2
  lattic      = lattice;
  lattic.nx   = geo.nx;
  lattic.ny   = geo.ny;
  lattic.symm = geo.symetric;
end
if nargout > 3
  reff = ref;
end
if nargout > 4
  geoo = geo;
end
