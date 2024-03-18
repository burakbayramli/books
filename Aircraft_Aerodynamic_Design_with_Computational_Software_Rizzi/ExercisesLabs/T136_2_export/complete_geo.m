function geoout = complete_geo(geoin)
% converts v 135 tornado-struct to include LE flaps
%JO 1604, 
% JOp 160618 - clear only data missing
global TraceOn
if TraceOn
    disp('T135/complete_geo');
end
%==== new fields =======
geoout = geoin;
% LE devices
if ~isfield(geoout,'flapped_LE')
    [nwing,npart]=size(geoout.fsym);
    geoout.wingside   = zeros(nwing,1);        % which side is unsymm. wing?
    geoout.flapped_LE = zeros(nwing,npart);    % partition has LE device?
    geoout.fnx_LE     = zeros(nwing,npart);    % number of panels chordwise on LE
    geoout.fc_LE      = zeros(nwing,npart);    % flap chords of LE devices (nwingxnelem)
    geoout.fsym_LE    = ones(nwing,npart);     % LE device deflects symmegeoout. or anti?
    geoout.flap_vector_LE  = zeros(nwing,npart); % how much deflection of LE device
end
% all-moving
if ~isfield(geoout,'mov_all')
    geoout.mov_all = zeros(nwing,npart);
    geoout.fsym_all= ones(nwing,npart);
    geoout.flap_vector_all= zeros(nwing,npart);
end
if ~isfield(geoout,'cslist')
    geoout.cslist     = {};      % cell array (1:# control surfs) of structs
    % cslist{k}.name: control surface name
    %          .defl          : deflection (degrees)
    %          .wingno        : wing number (sequence, 1..nwing)
    %          .type          :
    %                          1: LE flap,
    %                          2: TE flap
    %                          3: all-moving
    %          .part0         : partition-from(incl.)
    %          .part1         : partition-to  (incl.)
    %          .hingedata(1:3): for control surfaces defined by hinge lines
    %          (currently only all-moving)
    %          point on hinge axis, dihedral angle and
    %          sweep angle of axis, degrees
    %                 (1): c_frac, point is fraction c_frac from
    %                              apex on root chord
    %                 (2): dihedral angle of axis
    %                 (3): sweep angle of axis
    %          for ailerons, slats, elevators,rudders: empty
end
if ~isfield(geoout,'csname')
    geoout.csname     = cell(nwing,npart,3);    % Device names (nwing x nelem x 3) (1 LE, 2 TE, 3 all-mov)
end
if ~isfield(geoout,'hingepnt1')
    geoout.hingepnt1  = zeros(nwing,npart,3,3); % hingepoints for 1: LE, 2: TE, 3: All-mov
    geoout.hingepnt2  = zeros(nwing,npart,3,3); %    -"-
end
if ~isfield(geoout,'winglist')
    geoout.winglist   = cell(1,nwing);
end

