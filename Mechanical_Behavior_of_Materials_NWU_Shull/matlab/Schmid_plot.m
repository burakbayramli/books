figformat
schmid=[0.108 0.292 0.431 0.492 0.404 0.203 0.070];
normalstress=[21.3 70.4 105.9 152.7 344.2 848.2 2598];
appliedforce=[203.1 77.1 51.7 45.1 54.9 109 318.5];
appliedstress=1e-6*appliedforce/122e-6;
plot(appliedstress, schmid, '+b','markersize',18)
xlabel('applied stress (MPa)')
ylabel('Schmid factor')
print(gcf,'../figures/schmid_plot.eps','-depsc2')