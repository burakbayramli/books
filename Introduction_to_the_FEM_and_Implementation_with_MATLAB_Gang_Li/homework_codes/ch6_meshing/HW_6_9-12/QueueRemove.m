function [queue, head, tail, element] = QueueRemove(queue, head, tail)
if head>tail 
  element = nan;
else
  element = queue(head,:);
  head = head+1;
end