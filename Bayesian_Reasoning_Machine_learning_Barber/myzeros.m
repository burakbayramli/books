function out=myzeros(x)
%MYZEROS same as zeros(x) but if x is a scalar interprets as zeros([x 1])
if length(x)>1
    out=zeros(x);
else
    out=zeros([x 1]);
end