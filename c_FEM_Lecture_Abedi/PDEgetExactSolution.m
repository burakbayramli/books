function [ys, ysDot, hasExact, slnDGKappaYxsExact, hasExactYxsKappa] = PDEgetExactSolution(xs, loadCase, time, PDEtype)
if nargin < 1
    xs = [0, 1];
end
if nargin < 2
    loadCase = 0;
end
if nargin < 3
    time = 0;
end

if nargin < 4
    PDEtype = 0;
end

hasExact = 0;
ysDot = [];
slnDGKappaYxsExact = []; 
hasExactYxsKappa = 0;

npoints = length(xs);
ys = zeros(1, npoints);

if (loadCase == 0) % bar with T(0) = 0, q(L) = 1
    if (PDEtype == 0) % elliptic
        ys = -xs;
        hasExact = 1;
        slnDGKappaYxsExact = -ones(1, npoints);
        hasExactYxsKappa = 1;
    elseif (PDEtype == 2) % hyperbolic
        for i = 1:length(xs)
            x = xs(i);
            f = 1 - x - time;
            if (f > 0)
                ys(i) = -f;
                ysDot(i) = 1;
                slnDGKappaYxsExact(i) = 1;
            else
                ys(i) = 0;
                ysDot(i) = 0;
                slnDGKappaYxsExact(i) = 0;
            end
        end
        hasExact = 1;
        hasExactYxsKappa = 1;
    else
        hasExact = 0;
    end
elseif (loadCase == 1) % Q = sin(pi x)
    if (PDEtype == 0) % elliptic
        ys = 1/pi / pi * sin(pi * xs);
        hasExact = 1;
        slnDGKappaYxsExact = 1 / pi * cos(pi * xs);
        hasExactYxsKappa = 1;
    else
        hasExact = 0;
        hasExactYxsKappa = 0;
    end        
elseif (loadCase == 2) % Q = sin(pi/2 x)
    if (PDEtype == 0) % elliptic
        ys = (2/pi)^2 * sin(pi * xs / 2);
        slnDGKappaYxsExact = 2 / pi * cos(pi * xs / 2);
        hasExact = 1;
        hasExactYxsKappa = 1;
    else
        hasExact = 0;
        hasExactYxsKappa = 0;
    end        
elseif (loadCase == 3) % exact solution IC: sin(x) hyperbolic and parabolic with periodic BC - domain x = 0 - 2pi
    hasExact = 1;
    if (PDEtype == 0) % elliptic
        % exact sin is due to the source term
        ys = sin(xs);
    end
    if (PDEtype == 2) % hyperbolic
        ys = cos(time) * sin(xs);
        ysDot = -sin(time) * sin(xs);
        slnDGKappaYxsExact = cos(time) * cos(xs);
        hasExactYxsKappa = 1;
    elseif (PDEtype == 1) % parabolic
        ys = exp(-time) * sin(xs);
    end
end