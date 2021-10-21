% shift qtrue to proper position to plot, taking into account periodic BCs:
ntrue = length(qtrue);
qt2 = [qtrue;qtrue];
i1 = ntrue - floor((t-floor(t))*ntrue);

hold on
plot(xtrue,qt2(i1+1:i1+ntrue))
axis([0 1 -.5 1.5])
hold off

