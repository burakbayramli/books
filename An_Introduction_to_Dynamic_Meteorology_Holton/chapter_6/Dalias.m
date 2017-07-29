function [MTXout] = Dalias(MTXin,Nx,Ny)

% de-alias spectral products for the pseudo-spectral method
% 
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-09 09:31:05 -0800 (Thu, 09 Feb 2012) $
% $Revision: 788 $
% $Author: hakim $
% $Id: Dalias.m 788 2012-02-09 17:31:05Z hakim $
  
[My Mx] = size(MTXin);

%  adjust x-grid (cols)
%  put on bigger x-grid (cols)
if (Mx < Nx)
   MTXout = [MTXin(:,1:Mx/2) zeros(My,Nx-Mx+1) MTXin(:,Mx/2+2:end)];

%  put on smaller x-grid (cols)
elseif (Mx > Nx)
   MTXout = [MTXin(:,1:Nx/2) zeros(My,1) MTXin(:,Mx-(Nx/2-2):end)];

%  remove nyquist mode only (Mx=Nx)
else
   MTXout = [MTXin(:,1:Mx/2) zeros(My,1) MTXin(:,Mx/2+2:end)];
end

%  adjust y-grid (rows)
%  put on bigger y-grid (rows)
if (My < Ny)
   MTXout = [MTXout(1:My/2,:) ; zeros(Ny-My+1,Nx) ; MTXout(My/2+2:end,:)];

%  put on smaller x-grid (cols)
elseif (My > Ny)
   MTXout = [MTXout(1:Ny/2,:) ; zeros(1,Nx) ; MTXout(My-(Ny/2-2):end,:)];

%  remove nyquist mode only (My=Ny)
else
   MTXout = [MTXout(1:My/2,:) ; zeros(1,Nx) ; MTXout(My/2+2:end,:)];
end

return

