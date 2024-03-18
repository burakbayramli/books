function [startxyz,b,c,dihed,T,SW,TW] = wing_2_geo(wing)
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
nelem = 3;
if wing.winglet.Span > 0.001
    nelem = 4;
end
b = wingside*wing.Span*[wing.spanwise_kink1,...
                        wing.spanwise_kink2-wing.spanwise_kink1,...
                        1-wing.spanwise_kink2]/2;
if nelem > 3
    b(4) = wingside*wing.winglet.Span;
end

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

T = [wing.taper_kink1,...
     wing.taper_kink2/wing.taper_kink1,...
     wing.taper_tip/wing.taper_kink2  ];
if nelem > 3
    T(4) = wing.winglet.taper_ratio;
end

if wing.quarter_chord_sweep_inboard == 0
    wing.quarter_chord_sweep_inboard =180/pi*atan(tan(wing.LE_sweep_inboard *pi/180)-c/4/abs(b(1))*(1-T(1)));
    wing.quarter_chord_sweep_midboard=180/pi*atan(tan(wing.LE_sweep_midboard*pi/180)-c/4/abs(b(2))*(T(1)-wing.taper_kink2));
    wing.quarter_chord_sweep_outboard=180/pi*atan(tan(wing.LE_sweep_outboard*pi/180)-c/4/abs(b(3))*(wing.taper_kink2-wing.taper_tip));
end

SW = wingside*[wing.quarter_chord_sweep_inboard,...
    wing.quarter_chord_sweep_midboard...
    wing.quarter_chord_sweep_outboard];
if nelem > 3
    SW(4) = wingside*wing.winglet.quarter_chord_sweep;
end
SW = pi/180*SW;

rinc = 0;
TW = [wing.root_incidence,wing.kink1_incidence-rinc;...
    wing.kink1_incidence-rinc,wing.kink2_incidence-rinc;...
    wing.kink2_incidence-rinc,wing.tip_incidence-rinc;...
    ];
if nelem > 3
    TW(4,:) = [wing.winglet.root_incidence, wing.winglet.tip_incidence];
end
TW = pi/180*TW;

