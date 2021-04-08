function Aelem=fluidelemmatrix(coord8,coord4,u0,v0,Re)
Aelem=zeros(20,20);
A1=zeros(8,8);
A2=zeros(8,4);
A3=zeros(8,8);
A4=zeros(4,8);
A5=zeros(4,4);
A6=zeros(4,8);
A7=zeros(8,8);
A8=zeros(8,4);
A9=zeros(8,8);
Nx=5;Ny=5;x0=0;xf=1;y0=0;yf=1;
celem=connective(Nx,Ny);
elem8=[celem(:,1:8)];
elem4=[celem(:,9:12)];
nel=length(elem8(:,1));
u0=ones(8,1);
v0=zeros(8,1);
xp=[-sqrt(0.6) 0 sqrt(0.6)];
wp=[5/9 8/9 5/9];
[g1,g2]=meshgrid(xp,xp);
gp=[g2(:) g1(:)];
[w1,w2]=meshgrid(wp,wp);
w=w2(:).* w1(:);
ngp=length(gp);
for k=1:ngp
    ksi=gp(k,1);
    eta=gp(k,2);
    N(1,k)=-0.25*(1-ksi)*(1-eta)*(1+ksi+eta);
    N(2,k)=0.5*(1-ksi.^2)*(1-eta);
    N(3,k)=-0.25*(1+ksi)*(1-eta)*(1-ksi+eta);
    N(4,k)=0.5*(1+ksi)*(1-eta.^2);
    N(5,k)=-0.25*(1+ksi)*(1+eta)*(1-ksi-eta);
    N(6,k)=0.5*(1-ksi.^2)*(1+eta);
    N(7,k)=-0.25*(1-ksi)*(1+eta)*(1+ksi-eta);
    N(8,k)=0.5*(1-ksi)*(1-eta.^2);
    
    dN(1,1,k)=0.25*(1-eta)*(2*ksi+eta);
    dN(1,2,k)=-ksi*(1-eta);
    dN(1,3,k)=0.25*(eta-1)*(eta-2*ksi);
    dN(1,4,k)=0.5*(1-eta.^2);
    dN(1,5,k)=0.25*(eta+1)*(eta+2*ksi);
    dN(1,6,k)=-ksi*(eta+1);
    dN(1,7,k)=-0.25*(1+eta)*(eta-2*ksi);
    dN(1,8,k)=0.5*(-1+eta.^2);
    
    dN(2,1,k)=0.25*(1-ksi)*(2*eta+ksi);
    dN(2,2,k)=0.5*(-1+ksi.^2);
    dN(2,3,k)=0.25*(1+ksi)*(2*eta-ksi);
    dN(2,4,k)=-eta*(1+ksi);
    dN(2,5,k)=0.25*(1+ksi)*(ksi+2*eta);
    dN(2,6,k)=0.5*(1-ksi.^2);
    dN(2,7,k)=0.25*(1-ksi)*(2*eta-ksi);
    dN(2,8,k)=eta*(ksi-1);
    
    M(1,k)=0.25*(1-ksi)*(1-eta);
    M(2,k)=0.25*(1+ksi)*(1-eta);
    M(3,k)=0.25*(1+ksi)*(1+eta);
    M(4,k)=0.25*(1-ksi)*(1+eta);
    
    dM(1,1,k)=-0.25*(1-eta);
    dM(1,2,k)=0.25*(1-eta);
    dM(1,3,k)=0.25*(1+eta);
    dM(1,4,k)=-0.25*(1+eta);
    
    dM(2,1,k)=-0.25*(1-ksi);
    dM(2,2,k)=-0.25*(1+ksi);
    dM(2,3,k)=0.25*(1+ksi);
    dM(2,4,k)=0.25*(1-ksi);
end 
for k=1:ngp
    ubar=0;
    vbar=0;
    for i=1:8
        ubar=ubar+N(i,k)*u0(i);
        vbar=vbar+N(i,k)*v0(i);
    end
    Jacob8(:,:)=dN(:,:,k)*coord8(:,:);%Jacobian at kth gp
    detJacob8=abs(det(Jacob8));
    gdN=Jacob8(:,:)\dN(:,:,k); %2X8 change of variable in der. at kth gp
    gdN1=gdN(1,:);
    gdN2=gdN(2,:);
    for i=1:8
        for j=1:8
            A1(i,j)=A1(i,j)+w(k)*(N(i,k)*ubar*gdN1(j)...
                +N(i,k)*vbar*gdN2(j)...
                +(1/Re)*(gdN1(i)*gdN1(j)+gdN2(i)*gdN2(j)))*detJacob8;
        end
    end
    A9=A1;
    Jacob4(:,:)=dM(:,:,k)*coord4(:,:);
    gdM=Jacob4(:,:)\dM(:,:,k);
    gdM1=gdM(1,:);
    gdM2=gdM(2,:);
    detJacob4 = abs(det(Jacob4));
    for i=1:8
        for j=1:4
            A2(i,j)=A2(i,j)+w(k)*N(i,k)*gdM1(j)*detJacob4; 
            A8(i,j)=A8(i,j)+w(k)*N(i,k)*gdM2(j)*detJacob4; 
        end
    end
    Jacob4(:,:)=dM(:,:,k)*coord4(:,:);
    gdM=Jacob4(:,:)\dM(:,:,k);
    gdM1=gdM(1,:);
    gdM2=gdM(2,:);
    detJacob4=abs(det(Jacob4));
    for i=1:4
        for j=1:8
            A4(i,j)=A4(i,j)+w(k)*M(i,k)*gdN1(j)*detJacob4;
            A6(i,j)=A6(i,j)+w(k)*M(i,k)*gdN2(j)*detJacob4;
        end
    end
end
Aelem=[A1 A2 A3;A4 A5 A6;A7 A8 A9];
   
