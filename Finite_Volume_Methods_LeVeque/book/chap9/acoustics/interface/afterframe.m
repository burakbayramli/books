for mq1=mq
  subplot(length(mq),1,find(mq==mq1))
  hold on
  axis([-5 5 -.4 1.5])
  % dashed line to indicate interface:
  plot([0 0], [-.4 1.5], 'r--')
  hold off
  end

% for movie:
% makeframegif
