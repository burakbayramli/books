function gmmplot(M,V,W,D,S,C,Z)
% gmmplot(m,v,w,d,s,c,z)   Plot ellipse defining 2D gaussian
%     Plot an ellipse on the current figure corresponding to the 
%     first two dimensions of the multi-dimensional guassian defined 
%     by the mean vector m and the covariance matrix v (with weights w).
%     d (default [1 2]) defines which dimensions of m and v to use.
%     s (default 1) defines the radius of the ellipse in standard
%         deviations (or scales height of 1-D plots).
%     c (default 'r') defines the color/style of the plot.
%     If m is multi-row, plot multiple gausses, assume v is in 
%     raveled-row format.
%     If z is present, use plot3 and plot ellipses at Z value(s) in z.
% 1999jan06 dpwe@icsi.berkeley.edu uttclass
% $Header: $

if nargin < 3 | length(W) == 0
  W = ones(1,100);
end
if nargin < 4 | length(D) == 0
  D = [1 2];
end
if nargin < 5
  S = 1.0;
end
if nargin < 6
  C = 'r';
end

[nmix,ndim] = size(M);

oneD = 0;
if nargin < 7
  threeD = 0;
  if length(D) == 1
    oneD = 1;
  end
else
  threeD = 1;
  if length(Z) == 1
    Z = Z * ones(1,nmix);
  end
end

% Points to define a circle
npts = 20;
xy = [cos([0:npts]/npts*2*pi);sin([0:npts]/npts*2*pi)];

% Samples to define a 1d gauss
xx = -3:.1:3;
yymin = 1e14;
yymax = -1e14;

for mx = 1:nmix
  
  mm = M(mx,:);
  ww = W(mx);
  if nmix > 1 | size(V,1) == 1
    vv = reshape(V(mx,:),ndim,ndim);
  else
    vv = V;
  end

  if oneD

    hold on;
    yy = mm + xx*sqrt(vv);
    plot(yy, exp(-.5*xx.^2)/sqrt(vv)*ww*S,C);
    hold off
    yymin = min(min(yy), yymin);
    yymax = max(max(yy), yymax);
    
  else
    
    % Project onto dimensions
    [u,s,v] = svd(inv(vv(D,D)));

    rxy = S * v * inv(sqrt(s)) * xy;

    % Plot
    hold on
    if threeD
      plot3(mm(D(1))+rxy(1,:),mm(D(2))+rxy(2,:),Z(mx)*ones(1,npts+1),C)
    else
      plot(mm(D(1))+rxy(1,:),mm(D(2))+rxy(2,:),C)
    end
    hold off

  end

end

if oneD

  hold on;
  yy = yymin:((yymax - yymin)/100):yymax;
  plot(yy, S/max(W)*gmmval(yy,M,V,W));
  hold off

end
