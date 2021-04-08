%Finite element program for 2D steady incompressible 
%Navier-stokes 
clear all;
close all;
clc;
%Nx=3;Ny=3;x0=0;xf=3;y0=0;yf=0.6;
Nx=30;Ny=30;x0=0;xf=1;y0=0;yf=1;
%Nx=5,Ny=5,x0=0,xf=1,y0=-1,yf=0;

% MODIFY THIS TO TRY DIFF REYNOLDS NUMBERS
Re=[100];
%Re=[1 10 50 100 200 500 1000];
for j=1:length(Re)
    tol=1e-6;
    maxit=1000;
    erru=1;errv=1;
    vnd=(Nx+1)*(Ny+1);
    mnd=(Nx+1)*Ny+Nx*(Ny+1);
    nd=vnd+mnd;
    neq=2*nd+vnd;
    u0=ones(nd,1);
    v0=zeros(nd,1);
    celem=connective(Nx,Ny);
    elem8=[celem(:,1:8)];
    elem4=[celem(:,9:12)];
    gdof=fungdof(Nx,Ny,x0,xf,y0,yf);
    nel=length(elem8(:,1));
    node=node48(Nx,Ny,x0,xf,y0,yf);
    ne=length(celem(:,1));
    iter=0;
    tic
    while(or(erru>tol,errv>tol) & iter<=maxit)
        iter=iter+1
        %Step5. Assembly
        %Assembly matrix
        A=assemblymatrix(Nx, Ny, x0, xf, y0, yf, u0, v0, Re(j));
        %Assembly vector
        b=assemblyfluidvector(Nx,Ny,x0,xf,y0,yf,Re(j));
        dx=(xf-x0)/Nx;dy=(yf-y0)/Ny;
        bdof=funbdof(Nx,Ny);
        nb=10*(Nx+Ny);
        bv=zeros(nb,1);
        bv(2*Nx+4*Ny:4*(Nx+Ny))=1;
        %bv(4*(Nx+Ny)+1)=1;
        %step6. Applying Boundary Counditions
        upv=zeros(neq,1);
        nc=length(bdof);
        for i=1:nc
            id=bdof(i);
            upv(id)=bv(i);
        end
        freeNodes=setdiff(1:neq,bdof);
        b=b-A*upv
        %Step7. Solve the system
        upv(freeNodes)=A(freeNodes,freeNodes)\b(freeNodes);
        %Step8. Postprocessing
        u=upv(1:nd);
        p=upv(nd+1:nd+vnd);
        v=upv(nd+vnd+1:neq);
        erru=max(abs(u-u0))
        errv=max(abs(v-v0))
        uvel=(u+u0)/2
        vvel=(v+v0)/2;
        % invel(1:nd)=uvel;
        % invel(nd+vnd+1:neq)=vvel;
        % u0=invel(1:nd);
        % v0=invel(nd+vnd+1:neq)
        % u0=u;
        % v0=v;
        u0=uvel;
        v0=vvel;
    end

    fprintf('u-velocity    v-velocity\n')
    velocity=[u v];
    %disp(velocity);
    fprintf('pressure\n')
    %disp(p)
    figure
    % subplot(1,2,1)
    node=node48(Nx,Ny,x0,xf,y0,yf);
    % No idea why this line below isn't working
    %nodep=node4(Nx,Ny,x0,xf,y0,yf);
    x=node(:,1);
    y=node(:,2);
    L = sqrt(u.^2+v.^2);
    %disp(size(x))
    %disp(size(y))
    %disp(size(u./L))
    quiver(x,y,u./L,v./L,0.4);
    axis([0 1.1 0 1.1]);
    xlabel('x');
    title(['Re =', num2str(Re(j)) ',Velocity'])
    ylabel('y')
    view(0,90);
    x=x0:dx:xf;
    y=y0:dy:yf;
    [X,Y]=meshgrid(x,y);
    for i=1:Ny+1
        Mp(i,:)=p((Nx+1)*i-Nx:(Nx+1)*i);
    end
    % figure
    % surf(X,Y,Mp,'FaceColor','interp','EdgeColor','none');
    % title('Re=100');
    % h=colorbar;
    % set(get(h,'title'),'String','pressure');
    % xlabel('x');
    % ylabel('y');
    % view(0,90);
    figure 
    %subplot(1,2,2)
    
    % Last parameter below sets the number of contours
    contourf(X,Y,Mp,20);
    title(['Re=', num2str(Re(j))])
    h=colorbar;
    set(get(h,'ylabel'),'String','pressure');
    xlabel('x');
    ylabel('y')
    view(0,90);
end
toc
