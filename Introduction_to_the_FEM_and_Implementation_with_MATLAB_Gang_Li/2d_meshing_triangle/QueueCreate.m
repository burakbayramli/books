function [queue, head, tail] = QueueCreate(n_col)
  queue = zeros(10, n_col);             % initial queue size = 10
  head = 1; tail = 0;
end