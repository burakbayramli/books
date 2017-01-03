%2.2  trapcode.m

% inputs M,K,dt,n,V,U (initial values)
energy0 = V'*M*V+U'*K*U;  H = inv(M+K*dt*dt/4)*M;
for i=1:n % multiply (U,V) twice by block matrix, then by H=B^{-1}
  W = V-dt*inv(M)*K*U/2;   U = U+dt*V/2;
  V = W-dt*inv(M)*K*U/2;   U = U+dt*W/2;
  V = H*V;                 U = H*U;
end % compute the energy change at T=n*dt 
change = V'*M*V+U'*K*U - energy0 % change = 0, same energy
[U,V] % output U(T) and V(T)
