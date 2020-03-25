function x=betarnd(a,b)
% Beta(a,b) generator using generally applicable method (algorithm 4.25)
% vectors 'a' and 'b' have to be of the same size

x=nan(size(a));
for i=1:length(a)
    Y1=gamrand(a(i),1);
    Y2=gamrand(b(i),1);
    x(i)=Y1/(Y1+Y2);
end