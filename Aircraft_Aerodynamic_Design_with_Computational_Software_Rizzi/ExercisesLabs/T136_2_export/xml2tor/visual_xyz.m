function visual_xyz(fusegeo,xLEs,xTEs,xyzfoils,CSs,symwing,show_foil)
% Tornado function (internal): plot lifting surfaces and airfoils
% Input
%   xL      xyz of leading edge points
%   xTE     xyz of trailing edge points
%   xyzfoil coordinates of airfoils
%   CS      data structure for control surfaces
%
% Output
%   --
%
% calls
%   --
%--------------------------------------------------------------------------
%  Revisions: KTH 091022 v 0
% JO 160511 - compute xcent &c if not present
% JO 160618 - if fuse empty, don't show
%
global TraceOn
if TraceOn
    disp('xmlTorn/visual_xyz');
end
if nargin < 7
    show_foil = 1;
end
% planform: LE and TE
%1 fuselage
if ~isempty(fusegeo)
    try
        xcent  = fusegeo.xcent;
        zcent  = fusegeo.zcent;
        rzcent = fusegeo.rzcent;
        rycent = fusegeo.rycent;
    catch % for wireframe of fuse
        frames = fusegeo.frames;
        nf   = length(frames);
        xcent  = zeros(1,nf);
        zcent  = xcent;
        rzcent = xcent;
        rycent = xcent ;
        for i = 1:nf
            coords = frames{i};
            xcent(i)  = mean(coords(:,1));
            zcent(i)  = mean(coords(:,3));
            rycent(i) = (max(coords(:,2)) - min(coords(:,2)))/2;
            rzcent(i) = (max(coords(:,3)) - min(coords(:,3)))/2;
        end
    end
    % plot
    plot3(xcent,zeros(size(xcent)),zcent+rzcent,'k');
    hold on
    plot3(xcent, rycent,zcent,'r');
    plot3(xcent,-rycent,zcent,'r');
    plot3(xcent,zeros(size(xcent)),zcent-rzcent,'k');
    for k = 1:length(xcent)
        plot3([xcent(k) xcent(k)],[0 0],[zcent(k)+rzcent(k),zcent(k)-rzcent(k)],'k')
        plot3([xcent(k) xcent(k)],[rycent(k) -rycent(k)],[zcent(k),zcent(k)],'r')
        plot_xsect(xcent(k),zcent(k),rzcent(k),rycent(k))
    end
    if show_foil % also display surfaces on fuse
        n   = 20;
        fi = linspace(0,2*pi,n)';
        sfi = sin(fi);
        cfi = cos(fi);
        xpl = ones(n,1)*xcent;
        ypl = 0+sfi*rycent;
        zpl = ones(n,1)*zcent + cfi*rzcent;
        surfl(xpl,ypl,zpl,[1;1;1])
    end
end

nwing = length(xLEs);
for nw = 1:nwing
    xLE     = xLEs{nw};
    xTE     = xTEs{nw};
    xyzfoil = xyzfoils{nw};
    CS      = CSs{nw};
    nn  = size(xLE,1);
    tmp = [xLE;flipud(xTE)];
    plot3(tmp(:,1),tmp(:,2),tmp(:,3),'.k')
    hold on;
    for k = 1:nn-1
        plot3([xLE(k,1),xTE(k,1),xTE(k+1,1),xLE(k+1,1)],...
            [xLE(k,2),xTE(k,2),xTE(k+1,2),xLE(k+1,2)],...
            [xLE(k,3),xTE(k,3),xTE(k+1,3),xLE(k+1,3)],'k')
        %    patch([xLE(k,1),xTE(k,1),xTE(k+1,1),xLE(k+1,1)],...
        %      [xLE(k,2),xTE(k,2),xTE(k+1,2),xLE(k+1,2)],...
        %      [xLE(k,3),xTE(k,3),xTE(k+1,3),xLE(k+1,3)],'')
    end
    plot3(tmp(:,1),tmp(:,2),tmp(:,3),'k')
    % cross sections at kinks
    if show_foil
        for k = 1:nn
            plot3(xyzfoil{k}(:,1),xyzfoil{k}(:,2),xyzfoil{k}(:,3),'r')
            patch(xyzfoil{k}(:,1),xyzfoil{k}(:,2),xyzfoil{k}(:,3),'b')
            if k < nn
                vis_surf(xyzfoil{k},xyzfoil{k+1});
            end
        end
    end
    if symwing(nw)
        xLE(:,2)     = - xLE(:,2);
        xTE(:,2)     = - xTE(:,2);
        nn  = size(xLE,1);
        tmp = [xLE;flipud(xTE)];
        plot3(tmp(:,1),tmp(:,2),tmp(:,3),'.k')
        hold on;
        for k = 1:nn-1
            plot3([xLE(k,1),xTE(k,1),xTE(k+1,1),xLE(k+1,1)],...
                [xLE(k,2),xTE(k,2),xTE(k+1,2),xLE(k+1,2)],...
                [xLE(k,3),xTE(k,3),xTE(k+1,3),xLE(k+1,3)],'k')
            %    patch([xLE(k,1),xTE(k,1),xTE(k+1,1),xLE(k+1,1)],...
            %      [xLE(k,2),xTE(k,2),xTE(k+1,2),xLE(k+1,2)],...
            %      [xLE(k,3),xTE(k,3),xTE(k+1,3),xLE(k+1,3)],'')
        end
        plot3(tmp(:,1),tmp(:,2),tmp(:,3),'k')
        % cross sections at kinks
        sm = eye(3);
        sm(2,2) = -1;
        if show_foil
            for k = 1:nn
                plot3(xyzfoil{k}(:,1),-xyzfoil{k}(:,2),xyzfoil{k}(:,3),'r')
                patch(xyzfoil{k}(:,1),-xyzfoil{k}(:,2),xyzfoil{k}(:,3),'b')
                if k < nn
                    vis_surf(xyzfoil{k+1}*sm,xyzfoil{k}*sm);
                end
                
            end
        end
    end
    
    % Control surfaces
    if show_foil
        if ~isempty(CS)
            csxyz = CS.xyz;
            tmp=size(csxyz);
            ncs = tmp(1);
            for k = 1:ncs
                csnams = CS.csname{k};
                csnamp = CS.csname{k};
                if csnams(end)=='p'    % there was an s so show it
                    csnams(end) = 's';
                end
                switch csnams(1:3)
                    case 'fla'
                        col = 'r';
                    case 'ail'
                        col = 'g';
                    case 'sla'
                        col = 'b';
                    case 'rud'
                        col = 'y';
                    case 'ele'
                        col = 'c';
                    case 'can'
                        col = 'k';
                    otherwise
                        col = 'm';
                end
                np = CS.npts(k);
                txtcg = sum(csxyz(k,:,:),2)/np;
                plot3(csxyz(k,1:np,1),csxyz(k,1:np,2),csxyz(k,1:np,3),'k');
                patch(csxyz(k,1:np,1),csxyz(k,1:np,2),csxyz(k,1:np,3),col);
                text(txtcg(1),txtcg(2),txtcg(3),csnams);
                if symwing(nw)
                    csxyz(k,1:np,2)=-csxyz(k,1:np,2);
                    txtcg = sum(csxyz(k,:,:),2)/np;
                    plot3(csxyz(k,1:np,1),csxyz(k,1:np,2),csxyz(k,1:np,3),'k');
                    patch(csxyz(k,1:np,1),csxyz(k,1:np,2),csxyz(k,1:np,3),col);
                    text(txtcg(1),txtcg(2),txtcg(3),csnamp);
                end
            end
        else
            disp('no CS');
        end
    end
end