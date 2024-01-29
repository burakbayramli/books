function [queue, head, tail] = QueueAdd(queue, head, tail, element)
q_size=size(queue,1);
len= tail-head;
if len==-1                            % if queue is empty
  queue(1,:)=element; head=1; tail=0; 
end

% next 11 lines: check the size and location of the queue and adjust
if tail== q_size                       % if tail is close to the end
 if tail-head < 0.5*q_size             % if not even half-filled
    queue(1:tail-head+1,:)=queue(head:tail,:); % move queue forward
    head=1;
    tail=head+len;
 else                                  % if more than half filled
    t=zeros(2*q_size, size(queue,2));  % double the size
    t(1:tail,:)=queue;
    queue=t;
 end
end
tail=tail+1;                           % tail location + 1
queue(tail,:)=element;                 % add the element