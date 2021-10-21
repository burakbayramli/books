
clf

Haxes = axes('position',[.1 .1 .8 .3]);
set(Haxes,'fontsize',20)

for Frame=[0 1 2 4]
  for mq=1:2
    plotframe1
    if mq==1
       axis([-5 5 0.0 3.5])
       end
    if mq==2
       axis([-5 5 -0.5 2.0])
       end

    eval(['print dambreak_t' num2str(Frame) 'p' num2str(mq) ' -deps'])
    query
    end
  end

