
% first run with 12 output times up to t=1.5


for Frame=0:2:12
  for mq=1:2
    clf
    Haxes = axes('position',[.1 .1 .8 .3]);
    set(Haxes,'fontsize',15)
    plotframe1
    %axis([-1 1 -1 1])
    eval(['print raddam' num2str(Frame) 'q' num2str(mq) ' -deps'])
    query
    end
  end

