
function [N]=compLagIntp1D(start_x, end_x, x_vector, N, Nx)

  n=size(x_vector,1);
  for (i=1:n)
    N(1,i)=(x_vector(i) - end_x)/(start_x - end_x);
    N(2,i)=(x_vector(i) - start_x)/(end_x - start_x);
    Nx(1,i)=1/(start_x - end_x);
    Nx(2,i)=1/(end_x - start_x);
  end
