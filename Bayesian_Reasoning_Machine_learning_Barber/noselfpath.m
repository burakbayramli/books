function s = noselfpath(path)
%NOSELFPATH return a path excluding self transitions
s=path(1);
for i=2:length(path)
    if path(i)~=s(end)
        s = [s path(i)];
    end
end