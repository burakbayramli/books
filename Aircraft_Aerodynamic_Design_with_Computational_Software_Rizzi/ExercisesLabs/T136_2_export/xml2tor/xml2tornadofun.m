function [t,fusegeo,xLEs,xTEs,xyzfoils,CSs] = xml2tornadofun(ac,airfoil_lib_path)
% Tornado function: create tornado struct t from xml matlab struct ac
% Input
%   ac               CEASIOM aircraft geo struct
%   tornadohomedir   home directory of T135-003_EXPORT
%   datahomedir      where data should reside
%   airfoil_lib_path airfoil library, usually
%                    tornadohomedir/aircraft/airfoil (old organization)
%                                          or
%                    tornadohomedir/airfoil (new)
%   doplot           1: plot wireframe fuselage, lifting surfaces and airfoil profiles
%                    0: don't
% Output
%   t                New tornado struct
%
% calls
%   fusev               create fuselage geometry from ac
%   ar_centr            create contour lines (horizontal and vertical)
%   find_lift_surf      pick out lifting surfaces from ac
%   fuse_2_t            create tornado cruciform fuselage as wings
%   wing_2_t            create tornado wing struct
%   Htail_2_t           "       "      horizontal tail
%   Vtail_2_t           "       "      vertical tail
%   Canard_2_t          "       "      canard
%   insert_chords_wing  create partitions on wing
%   add_to              update pertitions to fit control surfaces
%   t2xyz               compute & plot xyz geometry incl. hinge points and
%
%--------------------------------------------------------------------------
%  Revisions: KTH 091022 v 0
%
fuse     = ac.Fuselage;
winglist = find_lift_surf(ac);
nwing    = length(winglist);
% assume npart <= 20
npart    = 20;
z_nw_np  = zeros(nwing,npart);
t.fnx        =   z_nw_np;              %number of panels on flap chords (2d array)
t.ny         =   z_nw_np;              %number of panels in span (2d array)
t.nx         =   z_nw_np;              %number of panels on chord (2d array)
t.fsym       =   ones(nwing,npart);    %flap deflection symmetry boolean bit  (2d array)
t.fc         =   z_nw_np;              %flap chord in percent of wingchord (2d array)
t.flapped    =   z_nw_np;              %flapped partition(wing part) boolean bit (2d array)
%wing shape
t.TW         =   zeros(nwing,npart,2); %partition twist (3d array)<1 inboard, 2 outboard>
t.T          =   z_nw_np;		       %Taper ratio (2d array)
t.SW         =   z_nw_np;		       %Sweep (2d array)
t.c          =   zeros(nwing,1);	   %Root chord (nwing)
t.dihed      =   z_nw_np;			   %Dihedral (nwing x nelem)
t.b          =   z_nw_np;			   %span(distance root->tip chord) (2d array)
t.symetric   =   zeros(nwing,1);	   %Wing symmetry boolean bit (2d array)
t.startx     =   zeros(nwing,1);	   %Partition starting coordinate (nwing)
t.starty     =   zeros(nwing,1);       % ---"----
t.startz     =   zeros(nwing,1);       % ---"----
t.nwing      =   nwing;	               %number of wings (scalar)
t.nelem      =   zeros(1,nwing);	   %number of partitions on each wing (1d array)
t.flap_vector=   z_nw_np;              %Flap deflection vector
t.ref_point  = [0 0 0];
t.CG         = [0 0 0];
t.meshtype   = 1;
t.nwing      = nwing;	               %number of wings (scalar)

%==== new fields =======
t.wingside   = zeros(nwing,1);         % non-symm. wing to starboard or port?
% LE:
t.flapped_LE = 0;                      % partition has LE device?
t.fnx_LE     = 0;                      % number of panels chordwise on LE
t.fc_LE      = [];                     % flap chords of LE devices (nwingxnelem)
t.fsym_LE    = ones(nwing,npart);      % LE device deflects symmet. or anti?
t.flap_vector_LE  = zeros(nwing,npart); % how much deflection of LE device
% all-moving
t.mov_all    = zeros(nwing,npart);      % is it all-moving?
t.fsym_all   = ones(nwing,npart);       % all-moving deflects symm. or anti?
t.flap_vector_all = zeros(nwing,npart); % how much deflection of all-moving?
t.cslist     = {};                      % cell array (1:# control surfs) of structs
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

t.csname    = cell(nwing,npart,3);    % Device names (nwing x nelem x 3) (1 LE, 2 TE, 3 all-mov)
t.hingepnt1 = zeros(nwing,npart,3,3); % hingepoints for 1: LE, 2: TE, 3: All-mov
t.hingepnt2 = zeros(nwing,npart,3,3); %    -"-
t.winglist  = cell(1,nwing);

L_scale   = fuse.Total_fuselage_length;
H_scale_F = fuse.Forefuse_X_sect_vertical_diameter;
H_scale_A = fuse.Aftfuse_X_sect_vertical_diameter;
H_scale   = H_scale_F;

nelmax = 0;
kkk    = 0;
for nw = 1:nwing
    wing = winglist{nw};
    % partition table:
    %=================
    % col 1-2: fraction span                - lin
    % col 3-4: dihedral (degrees)           - pccon
    % col 5-6: taper = chord/root chord     - lin
    % col 7-8: LE sweep angle degrees       - pccon
    % col 9-10: twist = incidence degrees   - lin
    % col 11-12: airfoils                   - pccon or lin
    % col 13-12+ncs: flap chord fractions   - lin?pccon?

    if wing.Span == 0
        wing.Span = sqrt(wing.area*wing.AR);
    end
    disp(['wing no ',num2str(nw),' : ',wing.type]);
    switch wing.type

        case 'fuseH' % cruciform wing, Hor.
            nytot = 4;
            [part_tab,c,startxyz,foil_list] = fuse_2_t(wing,'H');
            t.symetric(nw) = 1;

        case 'fuseV' % cruciform wing, Vert.
            nytot = 4;
            [part_tab,c,startxyz,foil_list] = fuse_2_t(wing,'V');
            t.symetric(nw) = 0;

        case 'wing'
            nytot = 15;
            if ~isfield(wing,'vertical_location')
                wing.vertical_location     = wing.placement*H_scale_A-H_scale_A/2;
                wing.longitudinal_location = wing.apex_locale*L_scale;
            end
            [part_tab,c,startxyz,foil_list] = wing_2_t(wing);
            disp(' xml2t 142')
            part_tab
            if wing.spanwise_kink2 == 1
                part_tab(end,:) = [];
            end
            if wing.spanwise_kink1 == 1
                part_tab(end,:) = [];
            end
            switch wing.configuration % new 0910
                case 0
                    t.symetric(nw) = 1;
                    t.wingside(nw) = 0;
                case -2
                    t.symetric(nw) = 0;
                    t.wingside(nw) = 1; % starboard
                case 2                  % but symetric can be only 0 or 1 ??
                    t.symetric(nw) = 0;
                    t.wingside(nw) = -1; % port
            end

        case 'Htail'
            nytot = 7;
            if ~isfield(wing,'vertical_location')
                %disp('xml2tfun 163')
                %ac.Horizontal_tail.empennage_layout
                ll = min(ac.Horizontal_tail.empennage_layout,1);
                if  ll == 0
                    wing.vertical_location     = wing.vertical_locale*H_scale_A;
                    wing.longitudinal_location = wing.apex_locale*L_scale;
                else % need to inspect the vertical tail (ugh!)
                    vtail = ac.Vertical_tail;
                    sk = vtail.spanwise_kink;
                    tk = vtail.taper_kink;
                    wing.vertical_location = vtail.vertical_locale*H_scale_A+...
                                             ll*vtail.Span;
                    tmp = (1              +tk)*(sk - 0)+...
                          (tk+vtail.taper_tip)*(1 - sk);
                    c   = 2*vtail.area/(tmp*vtail.Span);
                    lpos = vtail.apex_locale*L_scale + ...
                        min(ll,sk)*vtail.Span*tan(vtail.LE_sweep_inboard*pi/180);
                    cfrac = 1+ll/sk*(1-tk);
                    if ll > sk
                        lpos = lpos + ...
                            (ll-sk)*vtail.Span*tan(vtail.LE_sweep_outboard*pi/180);
                        cfrac = tk+ (ll-sk)/(1-sk)*(vtail.taper_tip-tk);
                    end
                    wing.longitudinal_location = lpos + 0.1*cfrac*c;
                end
            end
            [part_tab,c,startxyz,foil_list] = Htail_2_t(wing);
            t.symetric(nw) = 1;
            t.wingside(nw) = 0;
            if wing.spanwise_kink == 1
                part_tab(end,:) = [];
            end


        case 'Canard'
            nytot = 7;
            if ~isfield(wing,'vertical_location')
                wing.vertical_location     = wing.vertical_locale*H_scale;
                wing.longitudinal_location = wing.apex_locale*L_scale;
            end
            [part_tab,c,startxyz,foil_list] = Canard_2_t(wing);
            t.symetric(nw) = 1;
            t.wingside(nw) = 0;
            if wing.spanwise_kink == 1
                part_tab(end,:) = [];
            end


        case 'Vtail'
            nytot = 7;
            if ~isfield(wing,'vertical_location')
                wing.vertical_location     = wing.vertical_locale*H_scale;
                wing.longitudinal_location = wing.apex_locale*L_scale;
            end
            [part_tab,c,startxyz,foil_list] = Vtail_2_t(wing);
            t.symetric(nw) = 0;
            t.wingside(nw) = 1;
            if wing.spanwise_kink == 1
                part_tab(end,:) = [];
            end

        otherwise
            disp([' cannot parse ',wing.type]);
    end
    [eta1,header,csurfsym,defl,hingedata] = ...
        insert_chords_wing(wing,[part_tab(:,1);part_tab(end,2)]);
eta1
    ncs   = length(header);
    header
    csurfsym{:}
    nvar  = 6;
    ncols = nvar * 2;
    % pccon indicates whether panelwise constant or
    % lin interp between kinks
    pccon = [[0 0 1 1 0 0 1 1 0 0 0 0],ones(1,ncs)];
    %                             1 1  was ..

    % add the new breaks
    for k = 1:size(eta1,1)
        part_tab = add_to(part_tab,eta1(k,1),pccon);
    end
    
  disp('xml2t 247')
    part_tab
    nelem = size(part_tab,1);
    tmp   = zeros(nelem,3);
    oldname = ' ';
    % add flap chord fractions
    for k = 1:ncs
        k
        if k > 1
            oldname = header{k-1};
        end
        n   = length(oldname);
        kkk = kkk + 1;
        kk  = find(eta1(:,1+k));
        % all-moving wing columns have all flap chords zero!
        if isempty(kk)
            kk = [1 nelem+1]; % !!!
        end
        indx = kk(1):kk(end)-1; % the partitions in control surface k
        part_tab(indx,ncols+k)= eta1(indx,1+k);
        t.cslist{kkk}.name    = header{k};
        deflect               = defl{k,1};
        t.cslist{kkk}.defl    = deflect;
        t.cslist{kkk}.wingno  = nw;
        typ                   = defl{k,2};
        t.cslist{kkk}.type    = typ;
        t.cslist{kkk}.part0   = kk(1);
        t.cslist{kkk}.part1   = kk(end)-1;
        t.cslist{kkk}.hingedata = hingedata{k};
        disp('xml2 276');
        t.cslist{kkk}
        for jj = indx
            t.csname{nw,jj,typ}=header{k};
            disp(['nw ',num2str(nw),' jj ',num2str(jj),' ',num2str(csurfsym{k})])
            switch typ
                case 1
                    t.flap_vector_LE(nw,jj) = deflect;
                    t.fsym_LE(nw,jj)=csurfsym{k};
                case 2
                    t.flap_vector(nw,jj)    = deflect;
                    t.fsym(nw,jj)=csurfsym{k};
                case 3
                    t.flap_vector_all(nw,jj) = deflect;
                    t.mov_all(nw,jj)         = 1;
                    t.fsym_all(nw,jj)=csurfsym{k};
            end
        end
        % "sum" all flaps on TE and LE, OK since no overlap;
        % don't add the flaps on symmetric part if independent
        disp('xml2t 295')
        part_tab
        tmp
        oldname
        n
        header{k}
        if ~strncmpi(oldname, header{k},n) % JO 1009 ... n inst. of n -1
            tmp(:,typ) = tmp(:,typ)+part_tab(:,ncols+k);
        end
    end
    disp(['xml2tf 304 nw',num2str(nw)]);
    t.fsym(nw,:)
    % airfoils
    tfoil = part_tab(:,11:12);
    foil  = cell(nelem,2);
    for k = 1:nelem
        for k1 = 1:2
            tf = tfoil(k,k1);
            if tf == round(tf) % OK, get the foilname
                foil{k,k1} = foil_list{tfoil(k,k1)};
            else
                foil{k,k1} = num2str(100*nw+tf); % for later
            end
        end
    end

    row_nel=ones(nelem,1);
    t.nelem(nw) = nelem;
    %
    t.startx(nw) = startxyz(1);
    t.starty(nw) = 0;
    t.startz(nw) = startxyz(3);
    t.nx(nw,1:nelem)         =  8*row_nel;                       %number of panels on chord (2d array)
    t.fc_LE(nw,1:nelem)      =  tmp(:,1)';
    t.fc(nw,1:nelem)         =  tmp(:,2)';
    t.flapped(nw,1:nelem)    =  t.fc(nw,1:nelem)~=0;             %flapped partition(wing part) boolean bit (2d array)
    t.flapped_LE(nw,1:nelem) =  t.fc_LE(nw,1:nelem)~=0;
    t.fnx(nw,1:nelem)        =  3*t.flapped(nw,1:nelem);
    t.fnx_LE(nw,1:nelem)     =  3*t.flapped_LE(nw,1:nelem);

    t.TW(nw,1:nelem,:)       =  part_tab(:,9:10);                %partition twist (3d array)<1 inboard, 2 outboard>
    t.T (nw,1:nelem)         =  (part_tab(:,6)./part_tab(:,5))'; %Taper ratio (2d array)
    t.SW(nw,1:nelem)         =  (part_tab(:,7))';		         %Sweep (2d array)
    t.c(nw)                  =  c;                               %Root chord (nwing)

    % distribute elements spanwise - only linear so far
    bb                       =  ((part_tab(:,2)-part_tab(:,1))*wing.Span/2)';	    %span(distance root->tip chord) (2d array)
    if strcmp(wing.type,'Vtail')
        bb = 2*bb;
    end
    ntab                     =  round(bb/sum(bb)*nytot);
    ntab(ntab==0)            =  1; % at least one element ...
    t.ny(nw,1:nelem)         =  ntab';
    t.b(nw,1:nelem)          =  bb;
    t.dihed(nw,1:nelem)      =  (part_tab(:,3))';	                 %Dihedral (nwing x nelem)
    t.meshtype(nw,1:nelem)   =  1*row_nel;
    t.foil(nw,1:nelem,1:2)   =  foil;
    nelmax                   =  max(nelmax,nelem);
    t.winglist{nw}.name      =  winglist{nw}.name;
    t.winglist{nw}.seqno     =  winglist{nw}.seqno;
    t.winglist{nw}.type      =  winglist{nw}.type;
end % loop over wings

dum = cell(nw,nelmax,2);
for i = 1:nw
    for k = 1:2
        for j = 1:t.nelem(i)
            dum(i,j,k)=t.foil(i,j,k);
        end
        for j = t.nelem(i)+1:nelmax
            dum(i,j,k)= {' '};
        end
    end
end
t.foil=dum;

% adjust #columns in t.*
t.symetric   =   t.symetric';
t.ny         =   t.ny(:,1:nelmax);             %number of panels in span (2d array)
t.nx         =   t.nx(:,1:nelmax);             %number of panels on chord (2d array)
% about flaps
t.fnx        =   t.fnx(:,1:nelmax);            %number of panels on flap chords (2d array)
t.fsym       =   t.fsym(:,1:nelmax);           %flap deflection symmetry boolean bit  (2d array)
t.fc         =   t.fc(:,1:nelmax);             %flap chord in percent of wingchord (2d array)
t.flapped    =   t.flapped(:,1:nelmax);        %flapped partition(wing part) boolean bit (2d array)
t.flap_vector=   t.flap_vector(:,1:nelmax);    %Flap deflection vector
% wing shape
t.TW         =   t.TW(:,1:nelmax,:);           %partition twist (3d array)<1 inboard, 2 outboard>
t.T          =   t.T(:,1:nelmax);		       %Taper ratio (2d array)
t.SW         =   t.SW(:,1:nelmax);		       %Sweep (2d array)
t.dihed      =   t.dihed(:,1:nelmax);		   %Dihedral (nwing x nelem)
t.b          =   t.b(:,1:nelmax);			   %span(distance root->tip chord) (2d array)
% ==== new ====
t.fnx_LE        = t.fnx_LE(:,1:nelmax);        % d:o for LE flaps
t.fsym_LE       = t.fsym_LE(:,1:nelmax);
t.fc_LE         = t.fc_LE(:,1:nelmax);
t.flapped_LE    = t.flapped_LE(:,1:nelmax);
t.flap_vector_LE= t.flap_vector_LE(:,1:nelmax);
t.csname        = t.csname(:,1:nelmax,:);
t.mov_all       = t.mov_all(:,1:nelmax);
t.fsym_all      = t.fsym_all(:,1:nelmax);
t.flap_vector_all= t.flap_vector_all(:,1:nelmax);
%disp('xml2tfun 370')
t.fsym
t.csname
%pause
% compute geometry incl fuse and hinge data

[xcent,zcent,rzcent,rycent]=fusev(fuse);
fusegeo.xcent  = xcent;
fusegeo.zcent  = zcent;
fusegeo.rzcent = rzcent;
fusegeo.rycent = rycent;
[t,xLEs,xTEs,xyzfoils,CSs] = t2xyz(t,airfoil_lib_path);

