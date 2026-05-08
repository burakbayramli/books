function f = zero_cut_cells(tc, f)
% Zeros the distribution functions for the wall-opposite distribution
%   functions, so that wrong distributions are not received from the inner
%   cells of the wall (the non-fluid cells).
% tc: vector of handles to touched_cell objects.

% the unknown distributions are those with values >0 when dotted with 
%   surface normal. These should be set to zero after streaming and after
%   gathering, but before scattering!

% steps must take place in this order:
%   gather
%   stream
%   zero
%   scatter


for tck = 1:length(tc)
    for k = tc(tck).lattice_indices
        f(tc(tck).j, tc(tck).i, k) = 0;
    end
end