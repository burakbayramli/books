function resplot(resid)
%RESPLOT plot residuals computed by iterative solvers
%   resplot(resid);
%   input
%          resid        vector of residuals
%
%   IFISS function: AR, DJS, HCE, DJS; 17 January 2010.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

% check current plot status
stest = get(gcf,'Children');
if size(stest,1)~=0,
   % ask for plotting information
   figinfo=default('use new (enter figno) or existing (0) figure, default is 0',0);
   if figinfo==0,
      figno=default('figure number (default is current active figure)',gcf);
      if figno==0, figno=gcf; end
      % re-use existing figure
      figure(figno); hold on
   elseif figinfo>0 & floor(figinfo)==figinfo,
      figure(figinfo); hold off;
   else
      fprintf('Illegal figure number, generating new figure.\n');
      hold off; figure;
   end
end

its=length(resid)-1;
color=blanks(1);
colval=default('colour (b,g,r,c,m,y,k): enter 1--7 (default 1)',1);
Colour=['b','g','r','c','m','y','k','k'];
colour=Colour(rem(colval,length(Colour)));
if its>9,
semilogy([0:its],resid,[colour,'-']),
else, semilogy([0:its],resid,[colour,'.-']), end
axis('square'), xlabel('iterations'), ylabel(' log_{10}(residual)');
title('residual reduction')
hold on
figure(gcf);
