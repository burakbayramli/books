function tv(R,G,B)
  m,n = size(R)
  total = 0.
  for i=1:m-1
    for j=1:n-1
      total += norm([R[i,j]-R[i,j+1]; R[i,j]-R[i+1,j];
                     G[i,j]-G[i,j+1]; G[i,j]-G[i+1,j];
                     B[i,j]-B[i,j+1]; B[i,j]-B[i+1,j]])
    end
  end
  return total
end
