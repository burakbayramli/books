function [t,xLEs,xTEs,xyzfoils,CSs] = t2xyz(t,airfoil_lib_path)
% Tornado function (internal): compute xyz geometry for tornado struct t
% Input
%   t                tornado geo struct
%   airfoil_lib_path where are the airfoil files?
%   doplot           1: plot, 0: don't
%
% Output
%   t                updated  geo struct
%   xLE              xyz of leading edge points
%   xTE              xyz of trailing edge points
%   xyzfoil          airfoils
%   CS               data structure for control surfaces
%
% calls
%   visual_xyz       plot
%--------------------------------------------------------------------------
%  Revisions: KTH 091022 v 0
%  KTH 091110: default 0012 foil if file not found
global TraceOn
if TraceOn
    disp('xmlTorn/t2xyz');
end
[nwing,npart]=size(t.fc);
nwing = t.nwing;
CS = [];
CSs = cell(nwing,1);
xLEs= cell(nwing,1);
xTEs= cell(nwing,1);
xyzfoils= cell(nwing,1);

if nargin < 3
    doplot = 0;
end
% loop over wings
for nw = 1:nwing
    disp('t2xyz 36');
    nw
    nelem = t.nelem(nw);
    xLE = ones(nelem+1,1)*[t.startx(nw),t.starty(nw),t.startz(nw)];
    xTE = xLE;
    xQC = xLE;
    fac = t.c(nw);
    ii  = t.TW(nw,1,1);
    chords = fac;
    incs   = ii;
    xTE(1,:)=xLE(1,:)+fac*[cos(ii),0,-sin(ii)];
    xQC(1,:)=0.75*xLE(1,:)+0.25*xTE(1,:);
    for k = 1:nelem
        fi  = t.SW(nw,k);
        the = t.dihed(nw,k);
        s   = t.b(nw,k);
        xQC(k+1,:)=xQC(k,:)+s*[tan(fi),cos(the),sin(the)];
        fac       = fac*t.T(nw,k);
        chords    = [chords fac];
        ii        = t.TW(nw,k,2);
        incs      = [incs ii];
        xLE(k+1,:)=xQC(k+1,:) - 0.25*fac*[cos(ii),0,-sin(ii)];
        xTE(k+1,:)=xLE(k+1,:) + 4*(xQC(k+1,:)-xLE(k+1,:));
    end

    % get airfoils
    xyfoil = cell(nelem+1,1);

    for k = 1:nelem
        fcod = t.foil{nw,k,1};
        if isnan(str2double(fcod))  % filename
           %  disp('name');
            xyfoil{k}=profilegen(fullfile(airfoil_lib_path,fcod));
        else
            inum = str2double(fcod);
            if inum-floor(inum) == 0 % must be naca
                % disp('naca')
                xyfoil{k}=profilegen(fullfile(airfoil_lib_path,fcod));
            else
                 % disp('interp')
                xyfoil{k}=str2double(fcod);      % non-integer, interpolate
            end
        end
        if k == nelem
            xyfoil{k+1}=profilegen(fullfile(airfoil_lib_path,t.foil{nw,k,2}));
        end
    end
    xyfoil1 = xyfoil;
    for k = 1:nelem+1
        if length(xyfoil1{k})==1
            % interpolate
            fr = xyfoil1{k}-floor(xyfoil1{k});
            for j = 1:k
                if length(xyfoil1{k-j})>1
                    break
                end
            end
            klo = k - j;
            for j = 1:nelem+1-k
                if length(xyfoil1{k+j})>1
                    break
                end
            end
            khi = k+j;
           % xyfoil1{klo}, xyfoil1{khi}, chords(klo), chords(khi), fr
            xyfoil{k}=intrpfoil(xyfoil1{klo},chords(klo),xyfoil1{khi},chords(khi),fr);
          %  xyfoil{k}
        end
    end
    xyzfoil = cell(nelem+1,1);
    % compute wing cross sections from foil data
    for k = 1:nelem+1
        % scale and rotate
        % assuming LE is (0,0);
        xx = xyfoil{k}(2:end,1);
        yy = xyfoil{k}(2:end,2);
        sc = chords(k);
        xx = sc*xx;
        zz = sc*yy;
        fi = t.dihed(nw,max(k-1,1));
        % in dihed local system:
        yy = 0*zz;%.*sin(fi);
        % incidence
        c = cos(incs(k));
        s = sin(incs(k));
        tmp = xx;
        xx =  c*tmp + s*zz;
        zz = -s*tmp + c*zz;
        % rotate into global
        tmp = yy;
        yy  = cos(fi)*yy-sin(fi)*zz;
        zz  = sin(fi)*tmp + cos(fi)*zz;
        % translate to position
        xyzfoil{k}=[xx+xLE(k,1),yy+xLE(k,2),zz+xLE(k,3)];
    end

    %=================================
    % control surfaces
    CS = [];

    % TE devices (2)
    cs1 = t.fc(nw,1:nelem);
    ii  = find(cs1~= 0);
    ncsTE = 0;
    if ~isempty(ii)
        ncsTE = length(ii);
        for k = 1:ncsTE
            j = ii(k);
            CS.xyz(k,1,:) = xTE(j  ,:);
            CS.xyz(k,2,:) = xTE(j  ,:) + abs(cs1(j))*(xLE(j  ,:)-xTE(j  ,:));
            CS.xyz(k,3,:) = xTE(j+1,:) + abs(cs1(j))*(xLE(j+1,:)-xTE(j+1,:));
            CS.xyz(k,4,:) = xTE(j+1,:);
            CS.xyz(k,5,:) = xTE(j  ,:);
            CS.npts(k) = 5;
            CS.csname{k}    = t.csname{nw,j,2};
            t.hingepnt1(nw,j,2,:) = squeeze(CS.xyz(k,2,:));
            t.hingepnt2(nw,j,2,:) = squeeze(CS.xyz(k,3,:));
        end
    end

    % LE devices (1)
    cs1 = t.fc_LE(nw,1:nelem);
    ii = find(cs1~= 0);
    if ~isempty(ii)
        ncsLE = length(ii);
        for k = ncsTE+1:ncsTE+ncsLE
            j = ii(k-ncsTE);
            CS.xyz(k,1,:) = xLE(j  ,:);
            CS.xyz(k,2,:) = xLE(j  ,:) -cs1(j)*(xLE(j  ,:)-xTE(j  ,:));
            CS.xyz(k,3,:) = xLE(j+1,:) -cs1(j)*(xLE(j+1,:)-xTE(j+1,:));
            CS.xyz(k,4,:) = xLE(j+1,:);
            CS.xyz(k,5,:) = xLE(j  ,:);
            CS.npts(k)    = 5;
            CS.csname{k}  = t.csname{nw,j,1};
            t.hingepnt1(nw,j,1,:) = squeeze(CS.xyz(k,2,:));
            t.hingepnt2(nw,j,1,:) = squeeze(CS.xyz(k,3,:));
        end
    end

    xLEs{nw}=xLE;
    xTEs{nw}=xTE;
    xyzfoils{nw}=xyzfoil;
    CSs{nw}=CS;
end % loop over wings

%======================
% all-moving; compute & store hinge data
[dum ncstot] = size(t.cslist);
for k = 1:ncstot
    pntr = t.cslist{k};
    if pntr.type == 3 % all-moving
        nw   = pntr.wingno;
        x0   = [t.startx(nw),t.starty(nw),t.startz(nw)];
        x1   = x0 + t.c(nw)*[cos(t.TW(1,1)),0,-sin(t.TW(1,1))];
        th   = pntr.hingedata(2);
        fi   = pntr.hingedata(3);
        pnt1 = x0   + pntr.hingedata(1)*(x1-x0);
        pnt2 = pnt1 + [cos(th)*sin(fi) cos(th)*cos(fi) sin(th)];
        i0   = pntr.part0;
        i1   = pntr.part1;
        for kk = i0:i1
            t.hingepnt1(nw,kk,3,:) = pnt1;
            t.hingepnt2(nw,kk,3,:) = pnt2;
        end
    end
end
