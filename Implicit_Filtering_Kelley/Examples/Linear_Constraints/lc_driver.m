function [x, histout, histoutnew]=lc_driver
% LC_DRIVER
%
% Use imfil.m to solve the example in the Advanced Options section.
% Compare adding the direction v = (-1, 1)^T to the usual stencil
% with the vstencil option to (1) adding tangent directions as needed
% with the add_new_directions option and (2) a random search after
% the inner iteration using the explore option.
%
VS=[0 1; 0 -1; 1 0; -1 0; -1 1; 1 -1]';
x0=[1, 0]';
%
% Replace the next line with 
% options=imfil_optset;
options=imfil_optset('vstencil',VS);
% and the iteration will completely stagnate.
bounds=[0 0; 1 1]';
[x,histout,complete_history]=imfil(x0,@lc_obj,100,bounds,options);
%
% Alternatively you could add directions as needed.
%
options=imfil_optset('add_new_directions',@tangent_directions);
[x,histoutnew,complete_history]=imfil(x0,@lc_obj,100,bounds,options);
%
% Now compare the two approaches with a histout plot.
%
figure(1);
p1=subplot(1,1,1);
p2=semilogy(histout(:,1),max(1.d-15,histout(:,2)),'-',...
        histoutnew(:,1),max(1.d-15,histoutnew(:,2)),'--');
set(p2,'LineWidth',1.5,'Color','black');
set(p1,'FontSize',14,'XTick',[1 20 40 60],'XLim',[1 60],'YLim',[10^-7 30]);
legend('vstencil','new\_directions');
ylabel('Value of f');
xlabel('Cost');
