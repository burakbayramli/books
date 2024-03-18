function [yi,yip,yipp] = spleval(x,y,kk,xi)
% cubic interpolatory spline
% computes       yi = spline(xi)
% given slopes   kk = spline'(x)
% where spline(x) = y (ncols columns)
[dum,ncols]=size(y);
m = length(xi);
n = length(x)-1;
h = diff(x);
d = diff(y)./(h*ones(1,ncols));
[xs,ii]=sort(xi);
i  = 2;
is = 1;
yi = zeros(m,ncols);
yip  = yi;
yipp = yi;
while i <= n+1
    while xs(is) <= x(i)
        %       km = kk(i-1);
        %       kp = kk(i);
        t  = (xs(is)-x(i-1))/h(i-1);
        %disp([is i x(i-1) xs(is) x(i) t])
        a = kk(i-1,:)-d(i-1,:);
        b = kk(i  ,:)-d(i-1,:);
        yi(is,:)  = t*y(i,:)+(1-t)*y(i-1,:)+h(i-1)*t*(1-t)*(a*(1-t)-b*t);
        yip(is,:) = (y(i,:)-y(i-1,:))/h(i-1) + ...
                    (1-2*t)*(a*(1-t)-b*t)+ ...
                    t*(1-t)*(-a-b);
        yipp(is,:) = (-2*(a*(1-t)-b*t)+ ...
                      2*(1-2*t)*(-a-b))/h(i-1);                  
        is = is + 1;
        if is > m
            break
        end
    end
    if is > m
        break
    end
    i = i+1;
end
yi  = yi(ii,:);
yip = yip(ii,:);
