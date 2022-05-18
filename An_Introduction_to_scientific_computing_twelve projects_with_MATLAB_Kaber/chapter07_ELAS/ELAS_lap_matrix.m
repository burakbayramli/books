%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      function Ah5=ELAS_lap_matrix(hx,hy,nx,ny); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%    function Ah5=ELAS_lap_matrix(hx,hy,nx,ny)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%   Construction of the Laplacian operator matrix
%%   ELAS: elastic deformation of a thin plate
%%
%%                                        i+nx
%%                                         |
%%     5 points scheme              i-1 -- i -- i+1
%%                                         |
%%                                        i-nx
%%
%%
%%   Input   : hx, hy   mesh step size
%%             nx, ny   numbers of points 
%%
%%   Output  : Ah5      2D Laplacian operator matrix
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
      n=nx*ny;
      h2x=hx*hx;h2y=hy*hy;
      Ah5=sparse(n,n);
      Dx=toeplitz( [2.d0 -1.d0 zeros(1,nx-2) ] ) ;
      Dx = Dx / h2x ;
      Dy=eye(nx,nx) ;
      Dy = - Dy / h2y ;
      Dx = Dx - 2.d0 * Dy ;
      for k=1:(ny-1)
          i=(k-1)*nx ; j=k*nx ;
          Ah5( (i+1) : (i+nx) , (i+1) : (i+nx) ) = Dx ;
          Ah5( (j+1) : (j+nx) , (i+1) : (i+nx) ) = Dy ;
          Ah5( (i+1) : (i+nx) , (j+1) : (j+nx) ) = Dy ;
      end ;
      i=(ny-1)*nx ;
      Ah5( (i+1) : (i+nx) , (i+1) : (i+nx) ) = Dx ;
%%      figure(10);spy(Ah5);title('Matrix Ah5');
