function [ak,bl] = get_waves(k,l,kmax,lmax,facx,facy)

% compute x and y wavenumbers for use in spectral calculations.
%  
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-07 06:36:37 -0800 (Tue, 07 Feb 2012) $
% $Revision: 783 $
% $Author: hakim $
% $Id: get_waves.m 783 2012-02-07 14:36:37Z hakim $

  ak = facx*real(k - 1); bl = facy*real(l - 1);

  %     other spectral quadrants
  if ((k >= kmax) & (l <= lmax))
	 ak = -1.*facx*real(2*kmax - k + 1);
  elseif ((l >= lmax) & (k <= kmax)) 
	 bl = -1.*facy*real(2*lmax -l + 1);
  elseif ((k >= kmax) &  (l >= lmax))
	 ak = -1.*facx*real(2*kmax - k + 1);
	 bl = -1.*facy*real(2*lmax - l + 1);
  end
	 
