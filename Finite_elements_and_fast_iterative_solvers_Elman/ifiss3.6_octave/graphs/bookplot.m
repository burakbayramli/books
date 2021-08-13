function bookplot(fig, plotfile)
%BOOKPLOT saves IFISS figure as postscript file
%   bookplot(fig, plotfile);
%   input
%          fig        figure number of figure to be saved
%          plotfile   character string naming the file
%
%   uses the MATLAB script exportfig
%   IFISS function: DJS; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('\nCreating plotfiles ...')
gohome, cd plotfiles
figure(fig)
exportfig(gcf,[plotfile,'ef.eps'],'width',4.5,'fontmode','fixed',...
          'fontsize',8,'color','bw')
fprintf('\n%s',[plotfile,'ef.eps'])
exportfig(gcf,[plotfile,'ef.epsc'],'width',4.5,'fontmode','fixed',...
          'fontsize',8,'color','cmyk')
fprintf('\n%s\n',[plotfile,'ef.epsc'])
return
