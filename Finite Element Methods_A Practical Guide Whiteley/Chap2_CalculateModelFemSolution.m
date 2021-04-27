function [x,U] = Chap2_CalculateModelFemSolution(N)

  A = zeros(N+1,N+1);
  b = zeros(N+1,1);

  x = linspace (0, 1, N+1);
  
  for k =1: N
    h = 1/ N ;
    A_local = [1/ h , -1/ h ; -1/ h , 1/ h ];
    b_local = [ h ; h ];
    A (k : k +1, k:k +1) = A ( k : k +1 , k : k +1) + A_local ;
    b (k : k +1) = b(k:k+1) + b_local ;
  end

  A (1 ,:) = 0;
  A (1 ,1) = 1;
  b (1) = 0;
  A ( N +1 ,:) = 0;
  A ( N +1 , N +1) = 1;
  b ( N +1) = 0;

  U = A \ b ;
  aux=figure();
  plot (x , U , '-o')
  xlabel ( 'x')
  ylabel ( 'U')  
  saveas (1, "/tmp/out.png");
