if PlotType ~= 4
  axis([-2.5 2.5 -2.5 2.5])
  axis square
  end

if PlotType==4
  axis([0 2.5 0 2.1])
   dir = './1drad/';
   dim = 1;
   [amrdata1d,t1d] = readamrdata(dim,Frame,dir);
   if isempty(t1d)
      disp('Run xclaw in 1drad to generate 1d reference solution')
    else
      hold on;
      [q1d,x1d] = plotframe1ez(amrdata1d,mq,'r-');
      hold off;
    end
  end
