function [ score, pts ] = getHarrisCorners( img, std, kappa, th )

[xx, yy, xy] = getM(img, std);

% calc score
det = xx .* yy - xy.*xy;
trace2 = (xx + yy).^2;
score = det - kappa*trace2;


%imagesc((score > 0).*abs(score).^0.2)
%colormap gray


scoree = NaN(size(score)+2);
scoree(2:(end-1),2:(end-1)) = score;
[y, x] = find(score > scoree(1:(end-2), 2:(end-1)) ...
            & score > scoree(3:(end), 2:(end-1)) ...
            & score > scoree(2:(end-1), 1:(end-2)) ...
            & score > scoree(2:(end-1), 3:(end)) ...
            & score > th);

pts = [x y];
end