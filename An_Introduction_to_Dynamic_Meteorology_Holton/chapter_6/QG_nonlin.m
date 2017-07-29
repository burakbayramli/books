function out = QG_nonlin(x,y)

% compute a two-dimensional dealised product of spectral variables x and y.
% 
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-09 09:31:05 -0800 (Thu, 09 Feb 2012) $
% $Revision: 788 $
% $Author: hakim $
% $Id: QG_nonlin.m 788 2012-02-09 17:31:05Z hakim $
  
expand = 1.5; % fraction of extra points on big domain (default = 3/2);

Ny = size(x,1); Nx = size(x,2);
checkNy = size(y,1); checkNx = size(y,2);
if (Nx ~= checkNx) | (Ny ~= checkNy);
  disp(['Error in QG_nonlin: vectors have different length'])
end
  
% need to be very careful here with forward and backward ffts since
% matlab normalizes the ifft by the number of points. This must be 
% accounted for in ifft to physical space on the big grid by multiplying 
% by the following factor (ratio of number of points on the big grid to 
% the number of points on the small grid). Similarly, fft from the big 
% grid to the small grid (and truncating) requires dividing by fftfac.

fftfac = expand^2; 

% transform to big grid for nonlinear calc
xpad = Dalias(x,expand*Nx,expand*Ny);
ypad = Dalias(y,expand*Nx,expand*Ny);

% x and y on physical grid
xbig = ifft2(fftfac*xpad); 
ybig = ifft2(fftfac*ypad);

% nonlinear term in physical space. 
nonlin = xbig.*ybig;

% back to spectral space
tmp = fft2(nonlin/fftfac); 
out = Dalias(tmp,Nx,Ny);
