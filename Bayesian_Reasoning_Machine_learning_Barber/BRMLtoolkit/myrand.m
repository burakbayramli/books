function out=myrand(x)
%MYRAND same as rand(x) but if x is a scalar interprets as rand([x 1])
if length(x)>1
    out=rand(x);
else
    out=rand([x 1]);
end