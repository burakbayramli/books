%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      function Ah13=ELAS_bilap_matrix(hx,hy,nx,ny); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%    function Ah13=ELAS_bilap_matrix(hx,hy,nx,ny)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%   Construction of the bilaplacian operator matrix
%%   ELAS: elastic deformation of a thin plate
%%
%%
%%                                         i+2nx
%%                                          |
%%                                   i-1   i+nx  i+1
%%                                          |
%%     13 points scheme       i-2 -- i-1 -- i -- i+1 -- i+2 
%%                                          |
%%                                   i-1   i-nx  i+1
%%                                          |
%%                                         i-2nx
%%
%%   Input   : hx, hy   mesh step size
%%             nx, ny   numbers of points 
%%
%%   Output  : Ah13     2D Bi-Laplacian operator matrix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
      n=nx*ny;
      h2x=hx*hx;h2y=hy*hy;
      h4x=h2x*h2x;h4y=h2y*h2y;h4xy=h2x*h2y;
      c1=6.d0/h4x+6.d0/h4y+8.d0/h4xy;
      c2=-4.d0/h4x-4.d0/h4xy;
      c3=1.d0/h4x;
      D=toeplitz( [c1 c2 c3 zeros(1,nx-3) ] ) ;
      c4=-4.d0/h4y-4.d0/h4xy;
      c5=2.d0/h4xy;
      D1=toeplitz( [c4 c5 zeros(1,nx-2) ] ) ;
      c6=1.d0/h4y;
      D2=toeplitz( [c6 zeros(1,nx-1) ] ) ;
      for k=1:(ny-1)
          i=(k-1)*nx ; j=k*nx ;
          Ah13( (i+1) : (i+nx) , (i+1) : (i+nx) ) = D ;
          Ah13( (j+1) : (j+nx) , (i+1) : (i+nx) ) = D1 ;
          Ah13( (i+1) : (i+nx) , (j+1) : (j+nx) ) = D1 ;
      end ;
      i=(ny-1)*nx ;
      Ah13( (i+1) : (i+nx) , (i+1) : (i+nx) ) = D ;
      for k=2:(ny-1)
          i=(k-2)*nx ; j=k*nx ;
          Ah13( (j+1) : (j+nx) , (i+1) : (i+nx) ) = D2 ;
          Ah13( (i+1) : (i+nx) , (j+1) : (j+nx) ) = D2 ;
      end ;
%%      figure(10);spy(Ah13);title('Matrix Ah13');

