function qdanim(u,m,n)
%QDANIM Quadrilateral displacement animation for square model problem.
%   QDANIM(U) animates the displacement field U harmonically, illustrating
%   oscillating eigenfunctions. See QDPLOT for details on the plotting.
%
%   QDANIM(U,M,N) animates for M periods, with N steps per period.
%   The default values are M=3, N=8.
%
%   Example:
%      [x,y]=ndgrid(0:0.05:1,0:0.05:1);
%      U=0.1*[y(:).^2;sin(2*pi*x(:))];
%      qdanim(U)
%
%   See also: QDPLOT.

%   Per-Olof Persson <persson@math.mit.edu>

if nargin<2, m=3; end
if nargin<3, n=8; end

axmin=min([min(-u),min(u),min(1-u),min(1+u)]);
ax=[axmin,1-axmin,axmin,1-axmin];

for ii=1:m*n
  qdplot(u,sin(2*pi*ii/n));
  axis(ax);
  drawnow;
end
