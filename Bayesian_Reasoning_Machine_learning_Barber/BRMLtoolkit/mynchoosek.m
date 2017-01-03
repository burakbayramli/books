function out=mynchoosek(v,k)
%MYNCHOOSEK binomial coefficient v choose k
if isempty(k)||isempty(v)||k==0||length(v)<k
    out=[];
else
    out=nchoosek(v,k);
end