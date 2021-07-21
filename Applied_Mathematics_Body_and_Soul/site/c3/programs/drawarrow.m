function drawarrow(v1, v2)

v = v2 - v1;

a = 0.03 * norm(v);

n = cross([0, 0, 1], v);

if(norm(n) < 1e-4)
  n = cross([0, 1, 0], v);
end

n = n / norm(n);

drawline(v1, v2);
drawline(v2, v1 + 0.8 * v + a * n);
drawline(v2, v1 + 0.8 * v - a * n);
