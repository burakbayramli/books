function Q = PDE1DSourceData(xs, loadCase, PDEType, time)
if nargin < 1
    xs = [0, 1];
end
if nargin < 2
    loadCase = 0;
end
if nargin < 3
    time = 0;
end
npoints = length(xs);
Q = zeros(1, npoints); 

%loadCase == 0: q (flux) applied on the right edge
%loadCase == 4: 2 material example, stress load on the left edge
if ((loadCase == 0) || (loadCase == 4))
    return;
end

%loadCase == 3: exact solution IC: sin(x) hyperbolic and parabolic with periodic BC - domain x = 0 - 2pi
if (loadCase == 3) 
    if (PDEType == 0) % for elliptic case source term is applied
        Q = -sin(xs);
    else
        return;
    end
end

if (loadCase == 1)
    Q = sin(pi * xs);
elseif (loadCase == 2) % source term sin(pi/2 x), T(0) = 0, q(1) = 0 
    Q = sin(pi * xs / 2);
end
