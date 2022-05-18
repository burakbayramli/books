%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Matlab Solution of exercise 1 - project 7
%%   ELAS: elastic deformation of a thin plate
%%   Solution of the plate problem (linear equation)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      clear all; close all;
%
%     1) Construction of the linear system
%
%     number of points
      nx=20;ny=30;
%     step mesh size
%     dimensions of the plate 1 x 1 (without unit) 
      long=1.0;large=1.0;
      hx=long/(nx+1);hy=large/(ny+1);
      h2x=hx*hx;h2y=hy*hy;
      h4x=h2x*h2x;h4y=h2y*h2y;h4xy=h2x*h2y;
%     points coordinates 
      x=[0.:hx:long];y=[0.:hy:large];
      n=nx*ny;
      fprintf('\n ');
      fprintf('\n Points in x direction %d',nx);
      fprintf('\n Points in y direction %d',ny);
      fprintf('\n Total number of points %d',n);
      fprintf('\n ');
%     computing time measure 
      t=cputime;   
%     validation of the program 
%     with a well-known solution 
      u=zeros(nx+2,ny+2);uh=u;u2h=u;
      for ix=1:nx+2
      for iy=1:ny+2
         u(ix,iy)=ELAS_solution(x(ix),y(iy));
      end
      end
%     Laplacian operator matrix
      Ah5=ELAS_lap_matrix(hx,hy,nx,ny);
%     Bi-Laplacian operator matrix
      Ah13=ELAS_bilap_matrix(hx,hy,nx,ny);
%     Test problem matrix
      c1=1.0;c2=1.0;
      Ah=c2*Ah13+c1*Ah5;
%     Cholesky factorization - Matlab
      Lh=chol(Ah');
%     Display of matrix structure
      fs=18;
      figure(10);colormap('gray');
      spy(Ah5);title('Matrix Ah: 5-pts scheme','FontSize',fs);
      figure(11);colormap('gray');
      spy(Ah13);title('Matrix Ah: 13-pts scheme','FontSize',fs);
      figure(12);colormap('gray');
      spy(Lh);title('Cholesky factor Lh','FontSize',fs);
%     sampling of the right-hand side
      rhs_laph=zeros(nx,ny);rhs_bilaph=zeros(nx,ny);
      for ix=1:nx
         for iy=1:ny
%        Laplacian operator
         rhs_laph(ix,iy)=ELAS_lap_rhs(x(ix+1),y(iy+1));
%        Bi-Laplacian operator
         rhs_bilaph(ix,iy)=ELAS_bilap_rhs(x(ix+1),y(iy+1));
      end
      end
%
%     This part is requiered only in case of
%     non zero boundary values 
%
%     a) Dirichlet boundary condition
%
      for iy=1:ny+2
        uh(1,iy)=ELAS_solution(0.0,y(iy));
        uh(nx+2,iy)=ELAS_solution(1.0,y(iy));
        u2h(1,iy)=ELAS_solution(0.0,y(iy));
        u2h(nx+2,iy)=ELAS_solution(1.0,y(iy));
      end
      for ix=1:nx+2
        uh(ix,1)=ELAS_solution(x(ix),0.0);
        uh(ix,ny+2)=ELAS_solution(x(ix),1.0);
        u2h(ix,1)=ELAS_solution(x(ix),0.0);
        u2h(ix,ny+2)=ELAS_solution(x(ix),1.0);
      end
      gauche=zeros(ny+2,1);
      droite=zeros(ny+2,1);
      bas=zeros(nx+2,1);
      haut=zeros(nx+2,1);
      for iy=1:ny+2
        gauche(iy)=ELAS_solution(-hx,y(iy));
        droite(iy)=ELAS_solution(1.0+hx,y(iy));
      end
      for ix=1:nx+2
        bas(ix)=ELAS_solution(x(ix),-hy);
        haut(ix)=ELAS_solution(x(ix),1.0+hy);
      end
%
%     b) Correction of th right-hand side
%
%    Laplacian operator
     rhs_laph(1,1:ny)=rhs_laph(1,1:ny)+uh(1,2:ny+1)/h2x;
     rhs_laph(nx,1:ny)=rhs_laph(nx,1:ny)+uh(nx+2,2:ny+1)/h2x;
     rhs_laph(1:nx,1)=rhs_laph(1:nx,1)+uh(2:nx+1,1)/h2y;
     rhs_laph(1:nx,ny)=rhs_laph(1:nx,ny)+uh(2:nx+1,ny+2)/h2y;
%    Bi-Laplacian operator
     rhs_bilaph(1,1:ny)=rhs_bilaph(1,1:ny)-gauche(2:ny+1)'/h4x;
     rhs_bilaph(nx,1:ny)=rhs_bilaph(nx,1:ny)-droite(2:ny+1)'/h4x;
     rhs_bilaph(1:nx,1)=rhs_bilaph(1:nx,1)-bas(2:nx+1)/h4y;
     rhs_bilaph(1:nx,ny)=rhs_bilaph(1:nx,ny)-haut(2:nx+1)/h4y;
     rhs_bilaph(2,1:ny)=rhs_bilaph(2,1:ny)-uh(1,2:ny+1)/h4x;
     rhs_bilaph(nx-1,1:ny)=rhs_bilaph(nx-1,1:ny)-uh(nx+2,2:ny+1)/h4x;
     rhs_bilaph(1:nx,2)=rhs_bilaph(1:nx,2)-uh(2:nx+1,1)/h4y;
     rhs_bilaph(1:nx,ny-1)=rhs_bilaph(1:nx,ny-1)-uh(2:nx+1,ny+2)/h4y;
     rhs_bilaph(1,1:ny)=rhs_bilaph(1,1:ny)+uh(1,2:ny+1)*(4./h4x+4./h4xy);
     rhs_bilaph(nx,1:ny)=rhs_bilaph(nx,1:ny)+uh(nx+2,2:ny+1)*(4./h4x+4./h4xy);
     rhs_bilaph(1:nx,1)=rhs_bilaph(1:nx,1)+uh(2:nx+1,1)*(4./h4y+4./h4xy);
     rhs_bilaph(1:nx,ny)=rhs_bilaph(1:nx,ny)+uh(2:nx+1,ny+2)*(4./h4y+4./h4xy);
     for i=1:ny
        if (i>1)
        rhs_bilaph(1,i)=rhs_bilaph(1,i)-uh(1,i)*2./h4xy;
        rhs_bilaph(nx,i)=rhs_bilaph(nx,i)-uh(nx+2,i)*2./h4xy;
        end
        if (i<ny)
        rhs_bilaph(1,i)=rhs_bilaph(1,i)-uh(1,i+2)*2./h4xy;
        rhs_bilaph(nx,i)=rhs_bilaph(nx,i)-uh(nx+2,i+2)*2./h4xy;
        end
     end
     for i=1:nx
        if (i>1)
        rhs_bilaph(i,1)=rhs_bilaph(i,1)-uh(i,1)*2./h4xy;
        rhs_bilaph(i,ny)=rhs_bilaph(i,ny)-uh(i,ny+2)*2./h4xy;
        end
        if (i<nx)
        rhs_bilaph(i,1)=rhs_bilaph(i,1)-uh(i+2,1)*2./h4xy;
        rhs_bilaph(i,ny)=rhs_bilaph(i,ny)-uh(i+2,ny+2)*2./h4xy;
        end
     end
     rhs_bilaph(1,1)=rhs_bilaph(1,1)-uh(1,1)*2./h4xy;
     rhs_bilaph(nx,1)=rhs_bilaph(nx,1)-uh(nx+2,1)*2./h4xy;
     rhs_bilaph(1,ny)=rhs_bilaph(1,ny)-uh(1,ny+2)*2./h4xy;
     rhs_bilaph(nx,ny)=rhs_bilaph(nx,ny)-uh(nx+2,ny+2)*2./h4xy;
     rhs=zeros(nx*ny,1);
     for ix=1:nx
     for iy=1:ny
        rhs((iy-1)*nx+ix)=c2*rhs_bilaph(ix,iy)+c1*rhs_laph(ix,iy);
     end
     end
%
%    2) Solution of the linear system
%
     vh=Lh'\rhs;
     wh=Lh\vh;
     for ix=1:nx
     for iy=1:ny
         uh(ix+1,iy+1)=wh((iy-1)*nx+ix);
     end
     end
%    error computation
     e1=norm(u,2);
     e2=norm(u-uh,2);
     er=e2/e1;
     fprintf('\n Relative Error  %12.8f',er);
%    computing time measure 
     time=cputime-t;   
     fprintf('\n Computing time  %12.8f',time); 
%
%    3) Display of results
%
     fs=18;
%    exact solution 
     figure(13);
     colormap('gray');  
     surf(x,y,u');
     title('Exact solution','FontSize',fs);
%    computed solution   
     figure(14);
     colormap('gray');  
     surf(x,y,uh');
     title('Computed solution','FontSize',fs);
