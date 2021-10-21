
Haxes = axes('position',[.1 .1 .8 .3]);
set(Haxes,'fontsize',20)


for Frame=0:2:12
  for mq=1:2
    plotframe1
    %axis([-1 1 -1 1])
    eval(['print raddam' num2str(Frame) 'q' num2str(mq) ' -deps'])
    query
    end
  end

