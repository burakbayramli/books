function f = load_wall_distributions(f,saved,ci)
% saved: f distributions to set; each row of saved: 
%   [i,j, row of f corresponding to bounceback of ci]
% ci: wall lattice velocity indices

bi = bounceback_components(ci);
for k = 1:size(saved,1)
    i = saved(k,1);
    j = saved(k,2);
    f(j,i,bi) = reshape(saved(k,3:end),1,1,length(bi));
end