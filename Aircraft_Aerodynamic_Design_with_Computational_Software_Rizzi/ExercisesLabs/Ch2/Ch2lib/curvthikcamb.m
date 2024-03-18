function [xyc,thick,xyu,xyl,curv] = curvthikcamb(foil,n,dbg)
dbg
if isstr(foil)
  foil = foilfileread(foil);
end
% foil is [np,2] like ans mses blade file
[np,dum]=size(foil);
if np < 3 % make columns
    foil = foil.';
end
if nargin < 2
    n = 501; % odd: at least for symm. foils we get a single
             % point on LE
end
if nargin < 3
  dbg = 0;
end
dbg
yplus  = max(foil(:,2));
yminus = min(foil(:,2));
iplus  = find(foil(:,2)==yplus);
iminus = find(foil(:,2)==yminus);
if iminus < iplus   % reverse so :
                    % TE -> upper side -> LE -> lower side -> TE
  foil = flipud(foil);
end
s = [0;cumsum(sqrt(diff(foil(:,1)).^2+diff(foil(:,2)).^2))];
xy = interpspl(s,foil,linspace(0+1e-12,max(s)-1e-12,n));
curv = kapfun(xy);
% OK, so now split
ile = find(xy(:,1)==min(xy(:,1)));
xu = (xy(1:ile,1));
yu = (xy(1:ile,2));
xl = flipud(xy(ile:end,1));
yl = flipud(xy(ile:end,2));
tol = 1e-5
ds  = 0.017
if dbg
  figure(2)
  hold on
end

[xyc,thick,xyu,xyl] = meanC_ode_20(xu,yu,xl,yl,tol,ds,dbg);
%[xc,camby,thiky,xyu,xyl] = meancambery(xu,xl,yu,yl);

    