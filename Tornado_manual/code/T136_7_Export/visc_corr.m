function result=visc_corr(state,geo,lattice,ref)
% Function to calculte viscous corrections in TORNADO
% Input:
% state - TORNADO "state" structure
% geo -   TORNADO "geo" structure
% lattice - TORNADO "lattice" structure
% ref -     TORNADO "ref" structure
%
% result - output structure
% result.CN - normal force coefficient 
% result.CA - axial force coefficient 
% result.CS - side force coefficient
% result.Cl - rolling moment coefficient
% result.Cn - yawing moment coefficient
% result.Cm - pitching moment coefficient
% result.Cl_p - rolling moment due to roll rate derivative
% result.Cn_p - yawing moment due to roll rate derivative
% result.Cl_r -  rolling moment due to yaw rate derivative
% result.Cn_r - yawing moment due to yaw rate derivative

GRAD=180/pi;

% ########## Ref structure #############
Ref.Swn=ref.S_ref;
Ref.MAC=ref.C_mac;
Ref.Spn=ref.b_ref;
Ref.Xcg=geo.CG(1);
Ref.Ycg=geo.CG(3);
Ref.Zcg=geo.CG(2);

% ######### Input state ###############
    Mach=0.2;
    al=state.alpha*GRAD;
    p=.5*state.P*Ref.Spn;
    q=.5*state.Q*Ref.MAC;
    r=.5*state.R*Ref.Spn;
    
% ######### TsAGI lattice Structure ###############
n1=1;
for i=1:geo.nwing
    nx=geo.nx(i,1);
    nz=sum(geo.ny(i,:));
    if geo.symetric(i)==1
        ni=2*nx*nz;
    else
        ni=nx*nz;
    end
    n2=n1+ni-1;
    tsa_latt.colloc(:,1)=lattice.COLLOC(n1:n2,1);
    tsa_latt.colloc(:,3)=lattice.COLLOC(n1:n2,2);  %Russian coordinate system y=z
    tsa_latt.colloc(:,2)=lattice.COLLOC(n1:n2,3);

    [a b c]=size(lattice.VORTEX);

    try
        if b>5
            lattice.VORTEX(:,2,1)=lattice.VORTEX(n1:n2,4,1);
            lattice.VORTEX(:,3,1)=lattice.VORTEX(n1:n2,5,1);
        end
    end

    tsa_latt.vortex(:,1)=(lattice.VORTEX(n1:n2,2,1)+lattice.VORTEX(n1:n2,3,1))/2;
    tsa_latt.vortex(:,3)=(lattice.VORTEX(n1:n2,2,2)+lattice.VORTEX(n1:n2,3,2))/2;
    tsa_latt.vortex(:,2)=(lattice.VORTEX(n1:n2,2,3)+lattice.VORTEX(n1:n2,3,3))/2;

    tsa_latt.normal(:,1)=-lattice.N(n1:n2,1);
    tsa_latt.normal(:,3)=-lattice.N(n1:n2,2);
    tsa_latt.normal(:,2)=-lattice.N(n1:n2,3);
    
    for j=1:ni
        nj=n1+j-1;
        x(1,j)=lattice.VORTEX(nj,2,1);
        x(2,j)=lattice.VORTEX(nj,3,1);

        y(1,j)=lattice.VORTEX(nj,2,2);
        y(2,j)=lattice.VORTEX(nj,3,2);

        z(1,j)=lattice.VORTEX(nj,2,3);
        z(2,j)=lattice.VORTEX(nj,3,3);
    end
    
    dx=diff(x);
    dz=diff(z);
    dy=diff(y);

    tsa_latt.semisp=sqrt(dz.^2+dy.^2)./2;
    tsa_latt.vsweep=dx./tsa_latt.semisp;
   
    y_t(1)=geo.starty(i);
    j1=1;
    for j= 1:geo.nelem(i)
        y_t(2)=y_t(1)+geo.b(i,j);
        j2=j1+geo.ny(i,j)-1;
        del_y=(y_t(2)-y_t(1))/geo.ny(i,j);
        ytw=(y_t(1)+.5*del_y):del_y:y_t(2);
        tw0(1)=geo.TW(i,j,1);
        tw0(2)=geo.TW(i,j,2);
        tw=interp1(y_t,tw0,ytw);
        tsa_latt.twist(j1:j2)=GRAD*tw(:);
        y_t(1)=y_t(2);
        j1=j2+1;
    end
    
    clear x y z dx dy dz
    
    if geo.symetric(i)==1
        wing(i)=wing_lat1(i,geo,tsa_latt);
        n1=n1+2*nx*nz;
    else
        wing(i)=wing_lat0(i,geo,tsa_latt);
        n1=n1+nx*nz;
    end
    clear tsa_latt
end


% ######### 2D airfois data structure #############

for j=1:geo.nwing
    nz=sum(geo.ny(j,:));
    for i=1:geo.nelem(j)
       file1=char(geo.foil(j,i,1));
       file1=lower(file1);
       file1=strrep(file1,'.dat','.xfl');
       file2=char(geo.foil(j,i,2));
       file2=lower(file2);
       file2=strrep(file2,'.dat','.xfl');
       file1Full=fullfile(pwd,'aircraft','airfoil','xfl',file1);
       file2Full=fullfile(pwd,'aircraft','airfoil','xfl',file2);      
       wng1(i)=read_pol(file1Full);
       wng2(i)=read_pol(file2Full);
       if i==1
          alf_tmp=mas_cat(wng1(i).alf,wng2(i).alf);
       else
          alf_tmp=mas_cat(alf_tmp,wng1(i).alf);
          alf_tmp=mas_cat(alf_tmp,wng2(i).alf);
       end
    end
    
    nf=length(alf_tmp);
    foil(j).NF=nf;
    foil(j).ALF=alf_tmp;
    if geo.symetric(j)==1
        foil(j).CYAF=zeros(2*nz,nf);
        foil(j).CXAF=zeros(2*nz,nf);
        foil(j).MZF= zeros(2*nz,nf);        
    else
        foil(j).CYAF=zeros(nz,nf);
        foil(j).CXAF=zeros(nz,nf);
        foil(j).MZF= zeros(nz,nf);
    end
    
    k=0;
    z0=geo.starty(j);
    for i=1:geo.nelem(j) 
        CL1tmp=interp1(wng1(i).alf,wng1(i).CL,alf_tmp,'linear','extrap');
        CL2tmp=interp1(wng2(i).alf,wng2(i).CL,alf_tmp,'linear','extrap');
        CD1tmp=interp1(wng1(i).alf,wng1(i).CD,alf_tmp,'linear','extrap');
        CD2tmp=interp1(wng2(i).alf,wng2(i).CD,alf_tmp,'linear','extrap');
        CM1tmp=interp1(wng1(i).alf,wng1(i).CM,alf_tmp,'linear','extrap');
        CM2tmp=interp1(wng2(i).alf,wng2(i).CM,alf_tmp,'linear','extrap');
        z1=z0+geo.b(i);
        for jj=1:geo.ny(j,i)
            k=k+1;
            if geo.symetric(j)==1
                j1=nz+k; 
                j2=nz-k+1;
                z_tmp=wing(j).z_sec(j1);
                if (z0 < z_tmp) && ( z_tmp < z1)
                    del=(z_tmp-z0)/(z1-z0);
                    foil(j).CYAF(j1,:)=CL1tmp+del*(CL2tmp-CL1tmp);
                    foil(j).CXAF(j1,:)=CD1tmp+del*(CD2tmp-CD1tmp);
                    foil(j).MZF(j1,:)= CM1tmp+del*(CM2tmp-CM1tmp);
                    foil(j).CYAF(j2,:)=foil(j).CYAF(j1,:);
                    foil(j).CXAF(j2,:)=foil(j).CXAF(j1,:);
                    foil(j).MZF(j2,:)= foil(j).MZF(j1,:);
                end
            else
                z_tmp=wing(j).z_sec(k);
                if (z0 < z_tmp) && ( z_tmp < z1)
                    del=(z_tmp-z0)/(z1-z0);
                    foil(j).CYAF(k,:)=CL1tmp+del*(CL2tmp-CL1tmp);
                    foil(j).CXAF(k,:)=CD1tmp+del*(CD2tmp-CD1tmp);
                    foil(j).MZF(k,:)= CM1tmp+del*(CM2tmp-CM1tmp);
                end
            end
        end
        z0=z1;
    end
    clear alf_tmp CL1tmp CL2tmp CD1tmp CD2tmp CM1tmp CM2tmp wng1 wng2
end
        

% ######### Solution ##############################

% Successive approximation parameters
    Nap=7;
    mu=.1; 
    acc0=0.001; 
    nIterMax=300;
    dom=0.01;

% Matrix calculation  
    A=matr_nl(wing,Mach);
    Am1=inv(A);
    for i=1:geo.nwing
        wing(i).al_v=zeros(1,wing(i).Nsp);  
    end
    
for icase=1:3
    switch icase
        case 1
            p1=p; r1=r;
        case 2
            p1=p+dom; r1=r;
        case 3
            p1=p; r1=r+dom;
    end
% Successive iterations 
    ac=1;
    nIter=0;
    while (ac > acc0) && (nIter < nIterMax)
        nIter=nIter+1;
        b=rhs_nl(al,p1,q,r1,wing,Ref);
        g=Am1*b';        
        for i=1:geo.nwing
            wing(i).g_sec=circ_sec(i,g,wing);           
            wing(i).al_e=GRAD*wing(i).g_sec./wing(i).chord/pi;
            wing(i).al_ev=wing(i).al_e+wing(i).al_v+wing(i).twist;
            for j=1:wing(i).Nsp
                wing(i).cy_nl(j)=interp1(foil(i).ALF,foil(i).CYAF(j,:),wing(i).al_ev(j),'linear','extrap');
            end
            wing(i).g_sec_v=.5*wing(i).chord.*wing(i).cy_nl;
            wing(i).g_sec_v=appr(i,wing,Nap);
            acc(i)=max(abs(wing(i).g_sec-wing(i).g_sec_v));
        end
        acc(nIter)=max(acc);
        ac=acc(nIter);
        for i=1:geo.nwing
             wing(i).al_v= wing(i).al_v+mu*GRAD*( wing(i).g_sec- wing(i).g_sec_v)./ wing(i).chord;
        end
    end
    
    switch icase
        case 1
            coeff1=wing_loads(foil,wing,Ref);
        case 2
            coeff2=wing_loads(foil,wing,Ref);
        case 3
            coeff3=wing_loads(foil,wing,Ref);
    end

end
    
result.CN = coeff1.CNtot;
result.CA = coeff1.CAtot;
result.CS = coeff1.CStot;
result.Cl = coeff1.Cltot;
result.Cn = coeff1.Cntot;
result.Cm = coeff1.Cmtot;
result.Cl_p = (coeff2.Cltot-coeff1.Cltot)/dom;
result.Cn_p = (coeff2.Cntot-coeff1.Cntot)/dom;
result.Cl_r = (coeff3.Cltot-coeff1.Cltot)/dom;
result.Cn_r = (coeff3.Cntot-coeff1.Cntot)/dom;
 
% ##################################################
% ##################################################
% ##################################################

function output=wing_lat1(n,geo,latt)

nx=geo.nx(n,1);
nz=sum(geo.ny(n,:));
N=nx*nz;
N2=2*N;
nz2=nz*2;
output.Nch=nx;
output.Nsp=nz2;
output.colloc=zeros(N2,3);
output.vortex=zeros(N2,3);
output.normal=zeros(N2,3);
output.semisp=zeros(1,N2);
output.vsweep=zeros(1,N2);
output.cos_sw=zeros(1,N2);
output.chord=zeros(1,nz2);
output.twist=zeros(1,nz2);
output.z_sec=zeros(1,nz2);
output.dz=zeros(1,nz2);
output.x1q=zeros(1,nz2);
output.y1q=zeros(1,nz2);

n01=0; n02=0; n03=0; n04=0;
for j=1:geo.nelem(n)
    for i=1:geo.ny(n,j)
        isp_new=n01+i;
        isp_old=n02+i;
        ivr_new=n03+1+nx*(i-1);
        ivr_old=n04+1+nx*(i-1);
       
       b=nx*2*(latt.colloc(ivr_old,1)-latt.vortex(ivr_old,1));
       output.chord(nz+isp_new)=b;
       output.chord(nz-isp_new+1)=b;
       output.twist(nz+isp_new)=latt.twist(isp_new);
       output.twist(nz-isp_new+1)=latt.twist(isp_new);
       output.z_sec(nz+isp_new)=latt.vortex(ivr_old,3);
       output.z_sec(nz-isp_new+1)=-latt.vortex(ivr_old,3);
       output.dz(nz+isp_new)=2*latt.semisp(ivr_old);
       output.dz(nz-isp_new+1)=2*latt.semisp(ivr_old);
       output.x1q(nz+isp_new)=(1.5*latt.vortex(ivr_old,1)-.5*latt.colloc(ivr_old,1))+.25*b;
       output.x1q(nz-isp_new+1)=output.x1q(nz+isp_new);
       output.y1q(nz+isp_new)=latt.vortex(ivr_old,2);
       output.y1q(nz-isp_new+1)=latt.vortex(ivr_old,2);

        n1o=N+ivr_new;
        n2o=n1o+nx-1;
        n1=ivr_old;
        n2=n1+nx-1;
       output.colloc(n1o:n2o,:)=latt.colloc(n1:n2,:);
       output.vortex(n1o:n2o,:)=latt.vortex(n1:n2,:);
       output.normal(n1o:n2o,:)=latt.normal(n1:n2,:);
       output.semisp(n1o:n2o)=latt.semisp(n1:n2);
       output.vsweep(n1o:n2o)=latt.vsweep(n1:n2);
       output.cos_sw(n1o:n2o)=cos(atan(latt.vsweep(n1:n2)));
       n1o=N-nx+2-ivr_new;
       n2o=n1o+nx-1;
       output.colloc(n1o:n2o,1:2)=latt.colloc(n1:n2,1:2);
       output.colloc(n1o:n2o,3)=-latt.colloc(n1:n2,3); 
       output.vortex(n1o:n2o,1:2)=latt.vortex(n1:n2,1:2);
       output.vortex(n1o:n2o,3)=-latt.vortex(n1:n2,3);
       output.normal(n1o:n2o,1:2)=latt.normal(n1:n2,1:2);
       output.normal(n1o:n2o,3)=-latt.normal(n1:n2,3);
       output.semisp(n1o:n2o)=latt.semisp(n1:n2);
       output.vsweep(n1o:n2o)=-latt.vsweep(n1:n2);
       output.cos_sw(n1o:n2o)=cos(atan(-latt.vsweep(n1:n2)));
    end
    n01=n01+geo.ny(n,j);
    n02=n02+2*geo.ny(n,j);
    n03=n03+geo.ny(n,j)*nx;
    n04=n04+2*geo.ny(n,j)*nx;

end

% ##################################################

function output=wing_lat0(n,geo,latt)

nx=geo.nx(n,1);
nz=sum(geo.ny(n,:));
N=nx*nz;
output.Nch=nx;
output.Nsp=nz;
output.colloc=zeros(N,3);
output.vortex=zeros(N,3);
output.normal=zeros(N,3);
output.semisp=zeros(1,N);
output.vsweep=zeros(1,N);
output.cos_sw=zeros(1,N);
output.chord=zeros(1,nz);
output.twist=zeros(1,nz);
output.z_sec=zeros(1,nz);
output.dz=zeros(1,nz);
output.x1q=zeros(1,nz);
output.y1q=zeros(1,nz);

for i=1:nz
   n1=1+nx*(i-1);
   b=nx*2*(latt.colloc(n1,1)-latt.vortex(n1,1));
   output.chord(i)=b;
   output.twist(i)=latt.twist(i);
   output.z_sec(i)=latt.vortex(n1,3);
   output.dz(i)=2*latt.semisp(n1);
   output.x1q(i)=(1.5*latt.vortex(n1,1)-.5*latt.colloc(n1,1))+.25*b;
   output.y1q(i)=latt.vortex(n1,2);
   
   n2=nx*i;
   output.colloc(n1:n2,:)=latt.colloc(n1:n2,:);
   output.vortex(n1:n2,:)=latt.vortex(n1:n2,:);
   output.normal(n1:n2,:)=latt.normal(n1:n2,:);
   output.semisp(n1:n2)=latt.semisp(n1:n2);
   output.vsweep(n1:n2)=latt.vsweep(n1:n2);
   output.cos_sw(n1:n2)=cos(atan(latt.vsweep(n1:n2)));
end

% ##################################################

function output=read_pol(fname)

fid=fopen(fname,'r');
for i=1:12
    title1=fgetl(fid);
end
[tmp,count]=fscanf(fid,'%f %f',[7,inf]);
status=fclose(fid);
output.alf =tmp(1,:);
output.CL  =tmp(2,:);
output.CD  =tmp(3,:);
output.CM  =tmp(5,:);
clear tmp  

% ##################################################

function out=mas_cat(x,y)

tmp=sort([x, y]);
n=length(tmp);
out(1)=tmp(1);
j=1;
for i=2:n
    if tmp(i) >  tmp(i-1)
        j=j+1;
        out(j)=tmp(i);
    end
end

% ##################################################

function A=matr_nl(wing,Mach);

nw=length(wing);

i0=1;
for i=1:nw
    ni=wing(i).Nch*wing(i).Nsp;
    i1=i0;
    i2=i0+ni-1;
    lattice.semisp(i1:i2)=wing(i).semisp(1:ni);
    lattice.vsweep(i1:i2)=wing(i).vsweep(1:ni);
    lattice.vortex(i1:i2,1:3)=wing(i).vortex(1:ni,1:3);
    lattice.colloc(i1:i2,1:3)=wing(i).colloc(1:ni,1:3);
    lattice.normal(i1:i2,1:3)=wing(i).normal(1:ni,1:3);
    i0=i2+1;
end

n=length(lattice.semisp);
A=zeros(n,n);
for i=1:n
    for j=1:n
        % j - number of collocation point
        % i - numer of vortex
        x=lattice.colloc(j,1)-lattice.vortex(i,1);
        dy=lattice.colloc(j,2)-lattice.vortex(i,2);
        dz=lattice.colloc(j,3)-lattice.vortex(i,3);
        y=dy*lattice.normal(i,2)+dz*lattice.normal(i,3);
        z=-dy*lattice.normal(i,3)+dz*lattice.normal(i,2);
        [vy0,vz0]=sks_sta(x,y,z,lattice.semisp(i),lattice.vsweep(i),Mach);
        vy=vy0*lattice.normal(i,2)-vz0*lattice.normal(i,3);
        vz=vy0*lattice.normal(i,3)+vz0*lattice.normal(i,2);
        A(j,i)=vy*lattice.normal(j,2)+vz*lattice.normal(j,3);
    end
end

% ##################################################

function b=rhs_nl(al,p,q,r,wing,Ref)

GRAD=180/pi;

 wxr=p*2/Ref.Spn;
 wyr=-r*2/Ref.Spn;
 wzr=q*2/Ref.MAC;
 alr=al/GRAD;
 
 n=length(wing);
 i0=0;
 for i=1:n
     for j=1:wing(i).Nsp
            n1=i0+wing(i).Nch*(j-1)+1;
            n2=n1+wing(i).Nch-1;
            Vy=sin(alr)+wxr*wing(i).z_sec(j)+wzr*(wing(i).x1q(j)-Ref.Xcg);
            Vz=wxr*wing(i).y1q(j)+wyr*(wing(i).x1q(j)-Ref.Xcg);
            V=Vy*wing(i).normal(n1-i0,2)+Vz*wing(i).normal(n1-i0,3);           
            al_e=GRAD*atan(V/(cos(alr)+wyr*wing(i).z_sec(j)));
            al_tot=al_e-wing(i).al_v(j);
            for k=n1:n2
                b(k)=-2*pi*sin(al_tot/GRAD); 
            end
     end
     i0=i0+wing(i).Nch*wing(i).Nsp;
 end
     
% ##################################################

function [vy,vz]=sks_sta(x,y,z,l_2,tg,M)
% Calculation of steady downwash from horseshoe vortex
%
% Input:
% x - longitudinal coordinate of the collocation point with respect to the 
%     center of horseshoe vortex
% y - vertical coordinate of the collocation point with respect to the 
%     center of horseshoe vortex
% z - side coordinate of the collocation point with respect to the 
%     center of horseshoe vortex
% l_2 - semispan of horseshoe vortex
% tg - tangent of horseshoe vortex sweep
% M - flow Mach number 
%
% Output:
% vy - vertical downwash
% vz - side downwash
%

bet2=1-M*M;
bet2tg2=bet2+tg*tg;
z1=-l_2-z;
z2= l_2-z;
x1=x-tg*z;
y2=y*y;
r1=x1*x1+bet2tg2*y2;
r3=y2+z1*z1;
r4=y2+z2*z2;
a1=x1-z1*tg;
a2=x1-z2*tg;
r5=sqrt(a1*a1+bet2*r3);
r6=sqrt(a2*a2+bet2*r4);
vy=-z2/r4*((x1-z2*tg)/r6+1)+z1/r3*((x1-z1*tg)/r5+1);
vy=vy-x1/r1*((bet2tg2*z2-x1*tg)/r6-(bet2tg2*z1-x1*tg)/r5);
vy=.5*vy;
vz=1/r3-1/r4-(x1-z2*tg)/r6/r4+(x1-z1*tg)/r5/r3-tg/r1*((bet2tg2*z2-x1*tg)/r6-(bet2tg2*z1-x1*tg)/r5);
vz=.5*y*vz;

% ##################################################

function output=circ_sec(i,g,wing);

output=zeros(1,wing(i).Nsp);

if i>1
    ia=i-1;
    i0=0;
    for j=1:ia
        i0=i0+wing(j).Nch*wing(j).Nsp;
    end
    j0=0;
    for j=1:wing(i).Nsp
        j0=j0+1;
        n1=i0+wing(i).Nch*(j-1)+1;
        n2=n1+wing(i).Nch-1;
        output(j0)=sum(g(n1:n2));        
    end
else
    j0=0;
    for j=1:wing(i).Nsp
        j0=j0+1;
        n1=wing(i).Nch*(j-1)+1;
        n2=n1+wing(i).Nch-1;
        output(j0)=sum(g(n1:n2));
    end   
end

% ##################################################

function output=appr(i,wing,n)

if abs(wing(i).normal(1,3)) < 1
    Nap=n;
    nz=wing(i).Nsp;
    if Nap>=nz
        Nap=nz-3;
    end
    Sp=sum(wing(i).dz(:));
    tet=acos(2*wing(i).z_sec/Sp);
    X=zeros(Nap+1,nz);
    X(1,:)=1;
    for j=1:Nap
        X(j+1,:)=sin(j*tet);
    end

    [b,sb] = linreg(X,wing(i).g_sec_v');
    output=b'*X;

else
   output= wing(i).g_sec_v;
end    


% ##################################################

function [b,sb] = linreg(X,y)
%        Multiple linear regression
%
%        n - number of experimental observations
%        m - number of undependant variables
%        input:
%        X(m+1,n) - matrix of experimental plan
%        y(n,1) - experimental observations of dependant function
%
%        y=X(m+1,n)^T b(m+1,1) - model of experiment
%
%        output:
%        b - estimation of regression parameters
%        sb - standard deviations of parameters
%
%       A.A. Afifi, S.P. Azen. Statistical Analysis.
%       A Computer Oriented Approach.
%       Academic Press, New York, San Francisco, London, 1979
%
%	19.01.95
%
	[p,n]=size(X);
	cvb=inv(X*X');
	b= cvb*(X*y);
	dif=y-X'*b;
        s2=dif'*dif /(n-p);
	sb=diag(cvb)*s2;

% ##################################################
   
function output=wing_loads(foil,wing,Ref)

nw=length(wing);
GRAD=180/pi;
i0=0;
for i=1:nw
    cx=0;cy=0;cz=0;mx=0;my=0;mz=0;
    for j=1:wing(i).Nsp
        j1=i0+wing(i).Nch*(j-1)+1;
        dz=wing(i).dz(j);
        zs=wing(i).z_sec(j);
        ys=wing(i).y1q(j)-Ref.Ycg;
        xs=wing(i).x1q(j)-Ref.Xcg;
        bs=wing(i).chord(j);
        cyas=interp1(foil(i).ALF,foil(i).CYAF(j,:),wing(i).al_ev(j),'linear','extrap');
        cxas=interp1(foil(i).ALF,foil(i).CXAF(j,:),wing(i).al_ev(j),'linear','extrap');
        mzs=interp1(foil(i).ALF,foil(i).MZF(j,:),wing(i).al_ev(j),'linear','extrap');
        dcya=cyas*dz*bs;
        dcxa=cxas*dz*bs;
        csa=cos(wing(i).al_ev(j)/GRAD);
        sna=sin(wing(i).al_ev(j)/GRAD);
        dcy=dcya*csa+dcxa*sna;
        dcx=-dcya*sna+dcxa*csa;
        dcz=-dcy*wing(i).normal(j1,3);
        dcy=dcy*wing(i).normal(j1,2);
        dmz=mzs*dz*bs;
        cy=cy+dcy;
        cx=cx+dcx;
        cz=cz+dcz;
        mx=mx-dcy*zs+dcz*ys;
        my=my-dcx*zs-dcz*xs;
        mz=mz+dmz-dcy*xs+dcx*ys;
    end        
    i0=i0+wing(i).Nch*wing(i).Nsp;
    output.CN(i)=cy/Ref.Swn;
    output.CA(i)=cx/Ref.Swn;
    output.CS(i)=cz/Ref.Swn;
    output.Cl(i)=mx/Ref.Swn/Ref.Spn;
    output.Cm(i)=mz/Ref.Swn/Ref.MAC;
    output.Cn(i)=-my/Ref.Swn/Ref.Spn;
end

output.CNtot=sum(output.CN);
output.CAtot=sum(output.CA);
output.CStot=sum(output.CS);
output.Cltot=sum(output.Cl);
output.Cmtot=sum(output.Cm);
output.Cntot=sum(output.Cn);


% ##################################################
