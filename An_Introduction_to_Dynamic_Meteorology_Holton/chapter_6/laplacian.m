
function [lap] = laplacian(nlevs,func,fb,ft,DX,DY,dz)

%
% compute a 3D Laplacian using spectral methods in the horizontal and 2nd-order 
% finite differences in the vertical
% 
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-07 06:36:37 -0800 (Tue, 07 Feb 2012) $
% $Revision: 783 $
% $Author: hakim $
% $Id: laplacian.m 783 2012-02-07 14:36:37Z hakim $
  
% first z-loop for f_z at at intermediate levels
for k = 1:1:nlevs-1
  fz(k,:,:) = (func(k+1,:,:) - func(k,:,:)) / dz;
end

% second z-loop for laplacian at grid levels and f_zz; lap calc
for k = 1:1:nlevs
     
  fxx = ((i*DX).^2).*squeeze(func(k,:,:));
  fyy = ((i*DY).^2).*squeeze(func(k,:,:));
  
  if (k == 1) 
	 fzz = (squeeze(fz(k,:,:)) - fb) / dz;
  elseif (k == nlevs) 
	 fzz = (ft - squeeze(fz(k-1,:,:))) / dz;
  else
	 fzz = squeeze((fz(k,:,:) - fz(k-1,:,:))) / dz;
  end
  % this is critical to get the inversion to check; the mean is zero in the inversion
  fzz(1,1) = 0.;

  lap(k,:,:) = fxx + fyy + fzz;

end
