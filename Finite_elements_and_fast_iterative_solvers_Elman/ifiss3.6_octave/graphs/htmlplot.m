function htmlplot(fig, plotfile)
%HTMLPLOT saves IFISS figure as html file
%   htmlplot(fig, plotfile);
%   input
%          fig        figure number of figure to be saved
%          plotfile   character string naming the file
%
%   uses the MATLAB script exportfig
%   IFISS function: DJS; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('\nCreating plotfile ...')
gohome, cd plotfiles
figure(fig)
     exportfig(gcf,[plotfile,'ef.jpg'],'format','jpeg', 'width',4.5, ...
                'resolution',150,'fontmode','fixed', 'fontsize',8,'color','cmyk')
fprintf('\n%s\n',[plotfile,'ef.jpg'])
return
