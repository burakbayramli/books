function f = preload_inactive_cells(f, lasts)
% assume the surface is 45 degrees.

for k = 1:length(lasts)
    i = lasts(k);
    j = k;
    f(j,i,2) = f(j,i+1,2);
    f(j,i,3) = f(j+1,i,3);
    f(j,i,6) = f(j+1,i+1,6);
    if i-1 > 0
        f(j,i-1,2) = f(j,i,2);
        f(j,i-1,3) = f(j+1,i-1,3);
        f(j,i-1,6) = f(j+1,i,6);
    end
end