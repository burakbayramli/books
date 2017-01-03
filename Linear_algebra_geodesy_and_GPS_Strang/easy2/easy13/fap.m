% Script for plotting FA probabilities

%Kai Borre 13-06-2008
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 2008/06/13  $

set(0,'DefaultTextFontName','Times');
set(0,'DefaultAxesFontName','Times');
set(0,'DefaultTextFontSize',16);

false_alarm = [0.01, 0.00001, 0.00000001];
sigma_b = 1;
for i = 1:length(false_alarm)
    R(:,i) = sigma_b^2*sqrt(chi2inv(1-false_alarm(i),1:100));
end
h1 = plot(R);
set(h1,'linewidth',1);
xlabel('Degrees of Freedom  {\itm-n}','fontsize',16)
ylabel('Threshold  {\itR}','fontsize',16)
title('{\it\chi^2}-Probabilities','fontsize',16)
legend('{\itP}(FA | NC) = 10^{-2}',...
    '{\itP}(FA | NC) = 10^{-5}','{\itP}(FA | NC) = 10^{-8}')
set(gca,'fontsize',16)
box off
pause % opportunity to move legend
legend('boxoff')

print -depsc2 fap
%%%%%%%%%%%%%%%%%%%%%% fap.m  %%%%%%%%%%%