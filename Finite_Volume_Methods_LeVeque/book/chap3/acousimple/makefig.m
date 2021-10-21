
Haxes = axes('position',[.1 .1 .8 .3]);
set(Haxes,'fontsize',20)


%Htitle = title('velocity at time 0');
%set(Htitle,'fontsize',20)

for j=[0 1 5 10 15 20 25 30]
  for ip=1:2
    plotframe1(j,ip)
    axis([-1 1 -1 1])
    eval(['print acousimple_t' num2str(j) 'p' num2str(ip) ' -deps'])
    query
    end
  end

