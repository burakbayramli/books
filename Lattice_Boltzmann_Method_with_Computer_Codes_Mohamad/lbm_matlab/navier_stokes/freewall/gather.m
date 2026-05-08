function G = gather(f,weights,ci)
% computes the agglomerated distribution function for the volumetric 
%   boundary condition for each surfel.
% weights: assigned weights for each surfel as a 2d cell array:
%   surfels x components of touched_cells x 3 (i,j,weight)
% ci maps weights to f, by the proper component. ci(j) corresponds 
%   to weights (k,j)

G = zeros(size(weights,1),length(ci));
for s = 1:size(weights,1)
  for k = 1:length(ci)
    surfel = weights{s,k};
    touched_cells = size(surfel,1);
    for t = 1:touched_cells
        i = round(surfel(t,1));
        j = round(surfel(t,2));
        overlap_area = surfel(t,4);
        G(s,k) = G(s,k) + overlap_area*f(j,i,ci(k));
    end
  end
end




% Assumes one surface described by 'areas' (the relative portion of area of
%   every cell in the pgram).
% Assumes uniform grid; dh denotes the cell dimension.
% f is the distribution function for every cell corresponding to areas.
% gamma is the agglomeration, one entry for every z-dimensional entry in
%   areas.

% gamma = zeros(length(ci),1);
% for k = 1:length(ci)
%     gamma(k) = sum(sum(f(:,:,ci(k)).*areas(:,:,k)));
% end
