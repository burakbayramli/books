function XYZ = xml2geo(ac,doplot)
% Create polygons for ac silhouette from xml matlab struct ac
% Input
%   ac               CEASIOM aircraft geo struct
% Output
%   XYZ{1:nc}        cell array of xyz(1:np,1:3) curves  
%
% calls
%   fusev               create fuselage geometry from ac
%   find_lift_surf      pick out lifting surfaces from ac
%   wing_2_geo            create wing             geo
%   Htail_2_geo           "       horizontal tail geo
%   Vtail_2_geo           "       vertical tail   geo
%   Canard_2_geo          "       canard          geo
%   var2xyz               create xyz for lifting surf.
%--------------------------------------------------------------------------
%  Revisions: KTH 100530
%
if nargin < 2
    doplot = 1;
end
fuse     = ac.Fuselage;
winglist = find_lift_surf(ac)
nwing    = length(winglist);

L_scale   = fuse.Total_fuselage_length;
H_scale_F = fuse.Forefuse_X_sect_vertical_diameter;
H_scale_A = fuse.Aftfuse_X_sect_vertical_diameter;
H_scale   = H_scale_F;

nc    = 0;
XYZ = cell(nwing);
for nw = 1:nwing
    wing = winglist{nw};
    if wing.Span == 0
        wing.Span = sqrt(wing.area*wing.AR);
    end
    disp(['wing no ',num2str(nw),' : ',wing.type]);
    switch wing.type

        case 'wing'
            if ~isfield(wing,'vertical_location')
                wing.vertical_location     = wing.placement*H_scale_A-H_scale_A/2;
                wing.longitudinal_location = wing.apex_locale*L_scale;
            end
            [startxyz,b,c,dihed,T,SW,TW] = wing_2_geo(wing);
            switch wing.configuration % new 0910
                case 0
                    symetric = 1;
                    wingside = 0;
                case -2
                    symetric = 0;
                    wingside = 1; % starboard
                case 2                % but symetric can be only 0 or 1 ??
                    symetric  = 0;
                    wingside  = -1; % port
            end

        case 'Htail'
            if ~isfield(wing,'vertical_location')
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
            [startxyz,b,c,dihed,T,SW,TW] = Htail_2_geo(wing);
            symetric  = 1;
            wingside  = 0;

        case 'Canard'
            if ~isfield(wing,'vertical_location')
                wing.vertical_location     = wing.vertical_locale*H_scale;
                wing.longitudinal_location = wing.apex_locale*L_scale;
            end
            [startxyz,b,c,dihed,T,SW,TW] = Canard_2_geo(wing);
            symetric  = 1;
            wingside  = 0;

        case 'Vtail'
            if ~isfield(wing,'vertical_location')
                wing.vertical_location     = wing.vertical_locale*H_scale;
                wing.longitudinal_location = wing.apex_locale*L_scale;
            end
            [startxyz,b,c,dihed,T,SW,TW] = Vtail_2_geo(wing);
            symetric  = 0;
            wingside  = 1;

        otherwise
            disp([' cannot parse ',wing.type]);
    end

    XYZ1 = var2xyz(startxyz,b,c,dihed,T,SW,TW,symetric,wingside);
    nc1 = size(XYZ1,2);
    for n1 = 1:nc1
        nc = nc+1;
        XYZ{nc}=XYZ1{n1};
    end
end
% compute fuse geo
[xcent,zcent,rzcent,rycent]=fusev(fuse);
xcent  = xcent';
zcent  = zcent';
rzcent = rzcent';
rycent = rycent';
% fuselage curves
nc = nc+1;
XYZ{nc}=[xcent,zeros(size(xcent)),zcent+rzcent];
nc = nc+1;
XYZ{nc}=[xcent,rycent,zcent];
nc = nc+1;
XYZ{nc}=[xcent,-rycent,zcent];
nc = nc+1;
XYZ{nc}=[xcent,zeros(size(xcent)),zcent-rzcent];
npc = 30;
fi = linspace(0,2*pi,npc)';
sfi = sin(fi);
cfi = cos(fi);
for k = 1:length(xcent)
    nc = nc+1;
    XYZ{nc}=[[xcent(k);xcent(k)],[0; 0],[zcent(k)+rzcent(k);zcent(k)-rzcent(k)]];
    nc = nc+1;
    XYZ{nc}=[[xcent(k);xcent(k)],[rycent(k);-rycent(k)],[zcent(k);zcent(k)]];
    xpl = xcent(k)*ones(npc,1);
    ypl = 0+rycent(k)*sfi;
    zpl = zcent(k) + rzcent(k)*cfi;
    nc = nc+1;
    XYZ{nc}=[xpl,ypl,zpl];
end
if doplot
    for k = 1:nc
        xyzpl = XYZ{k};
        n = size(xyzpl,1);
        plot3(xyzpl(:,1),xyzpl(:,2),xyzpl(:,3),'k','linewidth',2);
        hold on
    end
    axis equal
end

