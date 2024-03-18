function [part_tab,c,startxyz,foil_list] = Vtail_2_t(wing)
% Tornado function (internal): extract info from Vtail wing struct
% Input
%   wing       CEASIOM aircraft wing struct
%
% Output
%   part_tab   table for partitions
%   c          chords at kinks
%   startxyz   wing apex coord
%   foil_list  airfoil profile files
%
% calls
% --
%--------------------------------------------------------------------------
%  Revisions: KTH 091022 v 0
%
startxyz = [wing.longitudinal_location,0,wing.vertical_location];
nelem = 2;
if wing.Span == 0
    wing.Span = sqrt(wing.area*wing.AR);
end
b = wing.Span*[wing.spanwise_kink,...
    1-wing.spanwise_kink];  %  note .... vtail!
% partition table:
%=================
% col 1-2: fraction span                - lin
% col 3-4: dihedral (degrees)           - pccon
% col 5-6: taper = chord/root chord     - lin
% col 7-8: LE sweep angle degrees       - pccon
% col 9-10: twist = incidence degrees   - lin
% col 11-12: airfoils                   - pccon
% col 13-12+ncs: flap chord fractions   -lin?pccon?
nvar     = 6;
ncols    = nvar * 2;
part_tab = zeros(nelem,ncols);
tmp      = cumsum(b);
part_tab(:,1:2) =[[0;tmp(1:end-1)'],tmp'];

% root chord from  area, tapers, and kinks
tk = wing.taper_kink;
tmp = (1           +tk)*(wing.spanwise_kink - 0)+...
    (tk+wing.taper_tip)*(1 - wing.spanwise_kink);
c   = 2*wing.area/(tmp*wing.Span);
T   = [tk,wing.taper_tip/tk];

if wing.quarter_chord_sweep_inboard == 0
    wing.quarter_chord_sweep_inboard = 180/pi*atan(tan(wing.LE_sweep_inboard *pi/180)-c/4/abs(b(1))*(1-T(1)));
    wing.quarter_chord_sweep_outboard= 180/pi*atan(tan(wing.LE_sweep_outboard*pi/180)-c/4/abs(b(2))*(T(1)-wing.taper_tip));
end

dihed = [90+wing.dihedral_inboard,...
    90+wing.dihedral_outboard];
dihed = pi/180*dihed;
part_tab(:,3:4)=dihed'*[1 1];

tmp = cumprod(T);
part_tab(:,5:6) = [[1;tmp(1:end-1)'],tmp'];

SW = [wing.quarter_chord_sweep_inboard,wing.quarter_chord_sweep_outboard];
SW = pi/180*SW;
part_tab(:,7:8) = SW'*[1 1];

TW = [wing.root_incidence,wing.kink_incidence;...
    wing.kink_incidence,wing.tip_incidence];
TW = pi/180*TW;
part_tab(:,9:10)=TW;

tfoil = [1,2;...
    2,3];
part_tab(:,11:12) = tfoil;
part_tab(:,1:2) = part_tab(:,1:2)/wing.Span;

foil_list = {'0012' '0012' '0012'}; % default
try     % one foil given
    foil_list = {wing.airfoil,wing.airfoil,wing.airfoil};
catch
end
try    % all foils given
    foil_list = {wing.airfoilRoot,wing.airfoilKink,wing.airfoilTip};
catch
end

end