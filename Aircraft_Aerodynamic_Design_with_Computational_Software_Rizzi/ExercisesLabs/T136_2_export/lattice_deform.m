function lattice_new=lattice_deform(geo,beam_p,defl_loc,rotmat,lattice,vCfraction,wingno)
% Deform lattice - i.e. XYZ, VORTEX, N, and COLLOC
lattice_new = lattice;

% Vortex points etc.
VORTEX = lattice.VORTEX;
npv    = size(VORTEX,2);
XYZ    = lattice.XYZ;
COLLOC = lattice.COLLOC;
N      = lattice.N;
[s1 s2 s3]=size(lattice.VORTEX);

%
nx = geo.nx(wingno,:)+geo.fnx(wingno,:);
ny = geo.ny(wingno,:);
npan = length(nx);           %number of wings and panels


cnx2=[];                        %mucking about with index variables
for j=1:npan
    if nx(j)>0
        cnx2=[cnx2 nx(j)];
    end
end
cny = ny.*(geo.symetric(wingno)+1); %corrected number of ypanels

ist = [0 cumsum(nx.*ny)]+1;
icorn      = zeros(4,length(ist)-1);
icorn(1,:) = ist(1:end-1);
icorn(2,:) = icorn(1,:) + nx(1:end-1);
icorn(3,:) = ist(2:end)-1;
icorn(4,:) = icorn(3,:) - nx(1:end-1);
xcorn = XYZ(icorn,1);
ycorn = XYZ(icorn,2);
zcorn = XYZ(icorn,3);
% 
m=0;
index2=0;
q=0;
for j=1:npan
    if cny(j)>1
        q=q+1;%loop per partition
    end
    Xc = xcorn(:,j);
    Yc = ycorn(:,j);
    Zc = zcorn(:,j);
    for k=1:cny(j)
        %per strip loop
        index1=index2+1;
        index2=index2+cnx2(q);
        np = index2-index1+1;
        m=m+1;
        vtrx = reshape(VORTEX(index1:index2,:,:),np*npv,3);
        xyz  = reshape(XYZ(index1:index2,:,:),np*5,3);
        coll = COLLOC(index1:index2,:);
        nrm  = N(index1:index2,:);
        pts  = [vrtrx; xyz; coll; nrm];
        
        
        %Local chord
        lemma1=localC1-localC2;
        lc=sqrt(sum(lemma1.^2));
        
        %local span
        lemma1=(-cornerp(1,:)+cornerp(2,:));
        lemma2=lemma1.*[0 1 1];%Disregarding x component
        ls(m)=sqrt(sum(lemma2.^2));
        
        %Strip Area
        la=ls(m)*lc;
        
        %
        %Forces
        F0(m)=sum(sqrt(F(index1:index2,2).^2+F(index1:index2,3).^2)); %Only Z and Y component
        
        h(:,1)=Mpoint(1)-pV(index1:index2,1);
        h(:,2)=Mpoint(2)-pV(index1:index2,2);
        h(:,3)=Mpoint(3)-pV(index1:index2,3);
        
        F3(m,:)=sum(F(index1:index2,:));
        M3(m,:)=sum(cross(F(index1:index2,:),h));
        
        clear h
       
    end
end
m=0;









