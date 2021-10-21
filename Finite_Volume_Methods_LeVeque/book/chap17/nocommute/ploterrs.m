
dx = 1./ [50 100 200 400];
errgs2 = [.0606 .015 .0042 .0012];
errss2 = [.0588 .0139 .0035 .000916];
errss1 = [.1505 .0705 .0382 .0197];

loglog(dx,errss1,'.','MarkerSize',22);
hold on
loglog(dx,errgs2,'o','MarkerSize',8)
loglog(dx,errss2,'+','MarkerSize',8)

hl = legend('Upwind with Strang splitting',...
       'Lax-Wendroff with Godunov splitting',...
       'Lax-Wendroff with Strang splitting');

hp =loglog(dx,errss1);
loglog(dx,errgs2)
loglog(dx,errss2)

xlabel('delta x','FontSize',15)
ylabel('max-norm of error','FontSize',15)
set(gca,'FontSize',15);
axis([1e-3 1e-1 1e-4 2])

hold off

print nocommuteerr -deps

