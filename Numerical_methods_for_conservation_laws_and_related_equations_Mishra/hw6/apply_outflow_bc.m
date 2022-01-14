function [ out ] = apply_outflow_bc( u )

out = u;

for i=1:size (u,1)
  out(i, 1) = out(i, 2);
  out(i, end) = out(i, end - 1);
end

end
