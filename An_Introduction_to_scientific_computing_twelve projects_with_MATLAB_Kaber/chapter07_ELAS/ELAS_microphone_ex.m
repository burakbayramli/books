%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Matlab Solution of exercise 2 - project 7
%%   ELAS: elastic deformation of a membrane
%%   Solution of the microphone problem (non-linear equation)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
     clear all; close all;
%
%    1) Construction of the linear system
%
%    number of points
     nx=20;ny=30;
%    step mesh size
%    dimensions of the plate 1 mm x 1 mm (unit m) 
     long=1.e-03;large=1.e-03;
     hx=long/(nx+1);hy=large/(ny+1);
     h2x=hx*hx;h2y=hy*hy;
     h4x=h2x*h2x;h4y=h2y*h2y;h4xy=h2x*h2y;
%    points coordinates 
     x=[0.:hx:long];y=[0.:hy:large];
     n=nx*ny;
     fprintf('\n ');
     fprintf('\n Points in x direction %d',nx);
     fprintf('\n Points in y direction %d',ny);
     fprintf('\n Total number of points %d',n);
     fprintf('\n ');
%    computing time measure 
     t=cputime;   
%    matrix of realistic problem
%    plate mechanical strain 
     T=100.; % in Newton/m 
%    plate Young modulus (silicium)
     E=1.3e+11; % in Newton/m2
%    plate thickness
     e=1.e-06; % 1 up to 10 microns  
%    Poisson coefficient
     nu=0.25; %  no dimension 
     c1=T;c2=(E*e*e*e)/(12.*(1.0-nu*nu));
%    Laplacian operator matrix
     Ah5=ELAS_lap_matrix(hx,hy,nx,ny);
%    Bi-Laplacian operator matrix
     Ah13=ELAS_bilap_matrix(hx,hy,nx,ny);
%    Test problem matrix
     Ah=c2*Ah13+c1*Ah5;
%    Cholesky factorization - Matlab
     Lh=chol(Ah');
%    Display of matrix structure
     fs=18;
     figure(10);colormap('gray');
     spy(Ah);title('Matrix Ah ','FontSize',fs);
     figure(11);colormap('gray');
     spy(Lh');title('Matrix Lh ','FontSize',fs);
%    sampling of the right-hand side
     rhs0=zeros(nx*ny,1);
     for ix=1:nx
     for iy=1:ny
        xx=x(ix+1);
        yy=y(iy+1);
        uu=0.;
        rhs0((iy-1)*nx+ix)=ELAS_pressure(xx,yy,uu);
     end
     end
%    boundary conditions 
     uh=zeros(nx+2,ny+2);
%    uh is null on the boundary ==> no correction of rhs (cf ELAS_plate_ex.m)
%
%    2) Solution of the linear problem
%
     vh=Lh'\rhs0;
     wh=Lh\vh;
     for ix=1:nx
     for iy=1:ny
         uh(ix+1,iy+1)=wh((iy-1)*nx+ix);
     end
     end
%
%    3) Display of results
%
%    capacitor thickness
     h=5.e-06; % 5 microns
     figure(11);
     colormap('gray');surf(x,y,h-uh');
     fs=18;
     title('Solution for the plate problem','FontSize',fs);
%
%    4)  Solution of the nonlinear problem
%
     eps0=1.0e-03;
     var=1.0;
%    initialisation 
     uuh=uh;                % uh = solution of linear problem
     rhs=zeros(nx*ny,1);
     k=0;
     while ( var > eps0 )
     k=k+1;
     uuha=uuh;
     for ix=1:nx
     for iy=1:ny
        xx=x(ix+1);
        yy=y(iy+1);
        uu=uuh(ix+1,iy+1);
        rhs((iy-1)*nx+ix)=ELAS_pressure(xx,yy,uu);
     end
     end
     vh=Lh'\rhs;
     wh=Lh\vh;
     for ix=1:nx
     for iy=1:ny
         uuh(ix+1,iy+1)=wh((iy-1)*nx+ix);
     end
     end
%    variation
     e1=norm(uuh,2);
     e2=norm(uuh-uuha,2);
     var=e2/e1;
     fprintf('\n Iteration  %d',k);
     fprintf('\n Variation  %12.8f',var);
     end
%    computing time measure 
     time=cputime-t;   
     fprintf('\n Computing time      %20.15f',time); 
%
%    5) Display of results
%
     figure(12);
     colormap('gray');surf(x,y,h-uuh');
     fs=18;
     title('Solution for the microphone problem','FontSize',fs);     
