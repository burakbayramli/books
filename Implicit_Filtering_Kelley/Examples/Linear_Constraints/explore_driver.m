function [xex, histoutex, xrand, histoutrand]=explore_driver
% EXPLORE_DRIVER
%
% Use imfil.m to solve the example in the Advanced Options section.
% Test of the explore option.
%
% Reinitialize the random number generator in MATLAB.
%
reset(RandStream.getDefaultStream);
%
x0=[1, 0]';
%
% Replace then next lime with 
% options=imfil_optset;
% and the iteration will completely stagnate.
npoints=10;
options=imfil_optset('explore_function',@random_search,'explore_data',npoints);
bounds=[0 0; 1 1]';
[xex,histoutex,complete_historye]=imfil(x0,@lc_obj,100,bounds,options);
%
% Reset the options and the random number generator.
%
options=imfil_optset('random_stencil',10);
reset(RandStream.getDefaultStream);
%
[xrand,histoutrand,complete_history]=imfil(x0,@lc_obj,100,bounds,options);
figure(2);
subplot(1,2,1)
plot_history(complete_historye,'Explore');
subplot(1,2,2)
plot_history(complete_history,'Random Stencil');
figure(1);
p1=subplot(1,1,1);
p2=semilogy(histoutex(:,1),max(10^-15,histoutex(:,2)),'-',...
            histoutrand(:,1),max(10^-15,histoutrand(:,2)),'--');
set(p2,'LineWidth',1.5,'Color','black');
set(p1,'FontSize',14,'XTick',[1 20 40 60 200],'XLim',[1 75],'YLim',[10^-7 30]);
legend('explore','random\_stencil');
ylabel('Value of f');
xlabel('Cost');

function plot_history(complete_history,labelh)
% PLOT_HISTORY
%
% Makes a scatter plot of the complete_history data structure
% if N=2.
%
[mv,nv]=size(complete_history.good_values);
plot(complete_history.good_points(1,:),complete_history.good_points(2,:),'*');
axis('square');
axis([0 1 0 1]);
title(labelh);

