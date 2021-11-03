function [vv] = exact_solution(t,x,alpha,beta)

% location of shock
if (pi+alpha*t>2*pi)
    d = pi+alpha*t - 2*pi*floor((pi+alpha*t)/(2*pi));
    x = x + 2*pi*floor((pi+alpha*t)/(2*pi));
else
    d = pi+alpha*t;
end

d2 = pi+alpha*t;

x1= x(x<d2);

if (~isempty(x(x<d2)))
    % coordinate transformation
    xn = x1-alpha*t;

    % solve burgers using the coordinate transformation
    u = burgers(beta,xn,t);
    v = alpha+u;

    x1 = x1 - 2*pi*floor((pi+alpha*t)/(2*pi));
    
    if (~isempty(x(x>d2)))
        x2 = x(x>=d2);
        x2 = flip(2*d2-x2);
        xn2 = x2-alpha*t;

        u2 = burgers(beta,xn2,t);
        v2 = alpha+u2;

        x2 = x2 - 2*pi*floor((pi+alpha*t)/(2*pi));
        xx = [x1; flip(2*d-x2)];

        vv = [v; flip(2*alpha-v2)];
    else
        xx = x1;
        vv = v;
    end
elseif (~isempty(x(x>d2)))
    x2 = x(x>=d2);
    x2 = flip(2*d2-x2);
    xn2 = x2-alpha*t;

    u2 = burgers(beta,xn2,t);
    v2 = alpha+u2;

    x2 = x2 - 2*pi*floor((pi+alpha*t)/(2*pi));
    xx = flip(2*d-x2);
    vv = flip(2*alpha-v2);
end


return


