function saved = save_wall_distributions(f,touched,ci)

% touched: cells that the wall passes through.
% ci: lattice velocity indices that are bounced back.


bi = bounceback_components(ci);
saved = zeros(size(touched,1), 2+length(bi));
for k = 1:size(touched,1)
    i = touched(k,1);
    j = touched(k,2);
    saved(k,:) = [i,j,reshape(f(j,i,bi),1,length(bi),1)];
end
