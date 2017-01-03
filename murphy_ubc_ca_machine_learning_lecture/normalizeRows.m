function [T,Z] = normalizeRows(T)

Z = sum(T,2); 
S = Z + (Z==0);
norm = repmat(S, 1, size(T,2));
T = T ./ norm;



