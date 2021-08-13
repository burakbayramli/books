function dttplot(dt,figno, marker)
%DTTPLOT plots timestep data 
%   dttplot(dt,figno,'bx');
%   input
%         dt         time step  vector
%         figno      figure number (not used)
%         marker     marker character string
%
%   IFISS function: DJS; 18 January 2010.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
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
ns=length(dt);
fprintf('%d timesteps\n',ns)
semilogy(2:ns-1,dt(2:end-1),marker,'MarkerSize',7)
title('Evolution of the time step','Color','black','FontSize',12)
ylabel('time step','Color','black');
xlabel('step','Color','black');
hold on
figure(gcf);
return