function [T0, Tdot0] = PDE1DICData(xs, loadCase, PDEtype)
if nargin < 1
    xs = [0, 1];
end
if nargin < 2
    loadCase = 0;
end
%loadCase == 0: q (flux) applied on the right edge
%loadCase == 4: 2 material example, stress load on the left edge
if ((loadCase == 0) || (loadCase == 4))
    sz = length(xs);
    T0 = zeros(1, sz);
    if (PDEtype > 0)
        Tdot0 = zeros(1, sz);
    end
    return;
end

Tdot0 = [];
time = 0;
[T0, Tdot0, hasExact, slnDGKappaYxsExact, hasExactYxsKappa] = PDEgetExactSolution(xs, loadCase, time, PDEtype);
if (hasExact == 1)
    return;
else
    printf(1, 'load case IC not implemented\n');
end
