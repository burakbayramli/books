function [part_tab,c,startxyz,foil_list] = wing_2_t(wing)
% Tornado function (internal): extract info from wing struct
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
switch wing.configuration
    case 0 % symmetric
        wingside = 1;
    case -2 % only starboard
        wingside = 1;
    case 2 % only port
        wingside = -1;
    otherwise
        disp(['wing config ',num2str(wing.configuration),...
            ' not known']);
end
% control surfaces
%(wing.winglet.present==1) && 
nelem = 3;
if (wing.winglet.Span > 0.001)
    nelem = 4;
end
b = wingside*wing.Span*[wing.spanwise_kink1,...
    wing.spanwise_kink2-wing.spanwise_kink1,...
    1-wing.spanwise_kink2]/2;
if nelem > 3
    b(4) = wingside*wing.winglet.Span;
end
% partition table:
%=================
% col 1-2: fraction span                - lin
% col 3-4: dihedral (degrees)           - pccon
% col 5-6: taper = chord/root chord     - lin
% col 7-8: LE sweep angle degrees       - pccon
% col 9-10: twist = incidence degrees   - lin
% col 11-12: airfoils                   - pccon
% col 13-12+ncs: flap chord fractions   - lin?pccon?
nvar = 6;
ncols = nvar * 2;
part_tab = zeros(nelem,ncols);
tmp = cumsum(b);
part_tab(:,1:2) =[[0;tmp(1:end-1)'],tmp'];

% root chord from  area, tapers, and kinks
tmp = (1               +wing.taper_kink1)*(wing.spanwise_kink1-                  0)+...
    (wing.taper_kink1+wing.taper_kink2)*(wing.spanwise_kink2-wing.spanwise_kink1)+...
    (wing.taper_kink2+wing.taper_tip  )*(1                  -wing.spanwise_kink2);
c   = 2*wing.area/(tmp*wing.Span);
dihed = wingside*[wing.dihedral_inboard,...
    wing.dihedral_midboard,...
    wing.dihedral_outboard];
if nelem > 3
    % there is a winglet defined by LE sweep and cant angle
    % root and tip incidence and
    dihed(4) = wingside*wing.winglet.Cant_angle;
end
dihed = pi/180*dihed;
part_tab(:,3:4)=dihed'*[1 1];

T = [wing.taper_kink1,...
    wing.taper_kink2/wing.taper_kink1,...
    wing.taper_tip/wing.taper_kink2  ];
if nelem > 3
    T(4) = wing.winglet.taper_ratio;
end
tmp = cumprod(T);
part_tab(:,5:6) = [[1;tmp(1:end-1)'],tmp'];

if wing.quarter_chord_sweep_inboard == 0
    %    disp(['set qsw ',num2str(wing.LE_sweep_inboard)])
    wing.quarter_chord_sweep_inboard =180/pi*atan(tan(wing.LE_sweep_inboard *pi/180)-c/4/abs(b(1))*(1-T(1)));
    wing.quarter_chord_sweep_midboard=180/pi*atan(tan(wing.LE_sweep_midboard*pi/180)-c/4/abs(b(2))*(T(1)-wing.taper_kink2));
    wing.quarter_chord_sweep_outboard=180/pi*atan(tan(wing.LE_sweep_outboard*pi/180)-c/4/abs(b(3))*(wing.taper_kink2-wing.taper_tip));
    % JOp 110618: if winglet, quarterchordsweep not defined
    if nelem > 3
        wing.winglet.quarter_chord_sweep = 0;
    end
end

SW = wingside*[wing.quarter_chord_sweep_inboard,...
    wing.quarter_chord_sweep_midboard...
    wing.quarter_chord_sweep_outboard];
if nelem > 3
    SW(4) = wingside*wing.winglet.quarter_chord_sweep;
end
SW = pi/180*SW;
part_tab(:,7:8) = SW'*[1 1];

rinc = 0;
TW = [wing.root_incidence,wing.kink1_incidence-rinc;...
    wing.kink1_incidence-rinc,wing.kink2_incidence-rinc;...
    wing.kink2_incidence-rinc,wing.tip_incidence-rinc;...
    ];
if nelem > 3
    TW(4,:) = [wing.winglet.root_incidence, wing.winglet.tip_incidence];
end
TW = pi/180*TW;
part_tab(:,9:10)=TW;

if nelem ==3
    tfoil = [1,2;...
        2,3;...
        3,4];
else
    tfoil = [1,2;...
        2,3;...
        3,4; ...
        4,5];
end
part_tab(:,11:12) = tfoil;
part_tab(:,1:2) = part_tab(:,1:2)*2/wing.Span;
foil_list = {'0012' '0012' '0012' '0012'};
if nelem > 3
    foil_list{end+1} = '0012';
end
try % one foil given, use for all
    foil_list = {wing.airfoil,...
        wing.airfoil,...
        wing.airfoil,...
        wing.airfoil};
    if nelem > 3
        foil_list{end+1} = wing.airfoil;
    end
catch
end
try  % foils given for each kink
    foil_list = {wing.airfoilRoot,...
        wing.airfoilKink1,...
        wing.airfoilKink2,...
        wing.airfoilTip};
    if nelem > 3
        foil_list{end+1} = wing.airfoilTip;
    end
catch
end
