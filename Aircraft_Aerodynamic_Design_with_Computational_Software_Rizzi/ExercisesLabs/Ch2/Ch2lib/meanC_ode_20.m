function [xyc,thick,xyu,xyl] = meanC_ode_20(xu,yu,xl,yl,tol,ds,dbg)
if dbg
  hold on
end
xu = xu(:); xl = xl(:); yu = yu(:); yl = yl(:);
if nargin < 7
  dbg = 0;
end
if nargin < 6
  ds = 0.02;
end
if nargin < 5
  tol = 1e-5;
end
% normalized arclength
su = [0;cumsum(sqrt(diff(xu).^2 + diff(yu).^2))];su = su/su(end);
sl = [0;cumsum(sqrt(diff(xl).^2 + diff(yl).^2))];sl = sl/sl(end);
% interpolating cubic splines
[dumu,kxyu] = interpspl(su,[xu,yu],linspace(0,1,200));
[duml,kxyl] = interpspl(sl,[xl,yl],linspace(0,1,200));
if dbg
    plot(dumu(:,1),dumu(:,2),'-.b');
    plot(duml(:,1),duml(:,2),'-.m');
end
% assume we start from common point

jend  = max(round(1/ds)+1,100);
ks    =(0:jend-1)/(jend-1);
slist = 1- (ks-1).^2;
xyc   = zeros(jend,2);
thick = zeros(jend,1);
% first pint
xyc(1,:) = [xu(1)+xl(1),yu(1)+yl(1)]/2;
thick(1) = sqrt((xu(1)-xyc(1,1))^2+ (yu(1)-xyc(1,2))^2);
[xyu,dxyu]= spleval(su,[xu,yu],kxyu,slist(2));
[xyl,dxyl]= spleval(sl,[xl,yl],kxyl,slist(2));
if dbg
    plot(xyu(1),xyu(2),'.k')
    plot(xyl(1),xyl(2),'.r')
end
% step along the lower curve, find point on upper
for j = 2:jend
  j
    s(2) = slist(j);
    s(2) = min(s(2),1-1e-5); % step along lower
    s(1) = s(2); % initial guess
    [xyl,dxyl] = spleval(sl,[xl,yl],kxyl,s(2));
    it = 0;
    err = 2*tol;
    % Newton iteration
    while err > tol
        it = it+1;
        [xyu,dxyu,ddxyu] = spleval(su,[xu,yu],kxyu,s(1));
        % position, tangent, and second derivative
        % find new su so that (xyu(su)-xyl) orth (xyu'(su)+ xyl')
        % f = (xyu(1)(s)-xyl(1))(dxyu(1)(s)+dxyl(1))+
        %     (xyu(2)(s)-xyl(2))*(dxyl(2)+dxyu(2)(s))= 0
        f = (xyu(1)-xyl(1))*(dxyu(1)+dxyl(1))+(xyu(2)-xyl(2))*(dxyl(2)+dxyu(2));
        dfds = dxyu(1)*(dxyu(1)+dxyl(1)) +...
            (xyu(1)-xyl(1))*ddxyu(1) + ...
            (xyu(2)-xyl(2))*ddxyu(2) + ...
            dxyu(2)*(dxyl(2)+dxyu(2));
        err  = abs(f/dfds);
        s(1) = s(1)-f/dfds;
        % don't overstep ...
        s(1) = min(max(s(1),0),1);
        if it > 10
          break
        end
    end
    % camber curve point
    xyc(j,:) = (xyu+xyl)/2;
    % thickness
    thick(j) = norm(xyu-xyc(j,:));
    if dbg
        plot(xyu(1),xyu(2),'.k')
        plot(xyl(1),xyl(2),'.r')
        plot(xyc(j,1),xyc(j,2),'ok')
        plot([xyu(1),xyl(1)],[xyu(2),xyl(2)],'r')
        title(num2str(it))
    end
    % stop in good time
    if xyc(j,1) < ds | it > 10
        break
    end
end
if it > 10
  disp('it too many')
end
if dbg
  axis equal
end
xyc = xyc(1:j-1,:);
thick = thick(1:j-1);