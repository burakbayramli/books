function q = ri1(data)
% 1st Riemann invariant for acoustics

q = -data(:,1) + 2*data(:,2);
end
