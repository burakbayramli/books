%K-SVD denoising example
% (using 8x8 patches)

p=imread('barbara.png');
[fc,fr]=size(p);
F0=im2double(p);
F0=F0*255;

sig=25; %noise var
etg=1.2*sig; %error target

Fn=F0+sig*randn(size(p)); %noisy image

K=256; %number of atoms in the final dictionary

%obtain from noisy image a collection of many patches
disp('obtaining many patches');
Npa=50000; %number of patches (you can add more)
PAM=zeros(64,Npa); %space for the matrix of patches

rPm=randperm(prod([fc,fr]-7)); %random permutation
spa=rPm(1:Npa); %selected patches
for nn=1:Npa,
  [r,c]=ind2sub([fc,fr]-7,spa(nn));
  aux=Fn(r:r+7,c:c+7); %a patch
  PAM(:,nn)=aux(:); %convert to column, insert into matrix
end;  

%compute an initial dictionary
aux=ceil(sqrt(K));
Di=zeros(8,aux);
for nn=0:aux-1,
    S=cos([0:1:7]'*nn*pi/aux);
    if nn>0, S=S-mean(S); end;
    Di(:,nn+1)=S/norm(S);
end;
Di=kron(Di,Di); %obtain a 64xK matrix

%dictionary normalization
Di=Di*diag(1./sqrt(sum(Di.*Di)));
Di=Di.*repmat(sign(Di(1,:)),size(Di,1),1);

%Apply KSVD-------------------------------------------------
disp('apply K-SVD...please wait')
for kk=1:4, %4 KSVD iterations (you can add more)
    CFM=spc(Di,PAM,etg); %sparse coding  
    %search for better dictionary elements (bDE)
    pr=randperm(K);
    for in=pr,
        %data indices that use jth dictionary elements
        rDI=find(CFM(in,:)); %relevant data indices       
        if(length(rDI)<1), 
            %when there are no such data indices
            aux=PAM-Di*CFM; aux2=sum(aux.^2);
            [d,i]=max(aux2);
            bDE=PAM(:,i);
            bDE=bDE./(sqrt(bDE'*bDE)); 
            bDE=bDE.*sign(bDE(1));
            CFM(in,:)=0;
        else    
            %when there are such data indices
            Maux=CFM(:,rDI);
            Maux(in,:)=0; %elements to be improved
            ers=PAM(:,rDI)-Di*Maux; %vector of errors to minimize
            [bDE,SV,bV]=svds(ers,1); %the SVD
            CFM(in,rDI)=SV*bV'; %use sign of first element
        end;      
       
        Di(:,in)=bDE; %insert better dictionary element;
    end;
    disp(['iteration: ',num2str(kk)]);
end;

%clean dictionary
B1=3; B2=0.99;
%removing of identical atoms
er=sum((PAM-Di*CFM).^2,1);
Maux=Di'*Di; Maux=Maux-diag(diag(Maux));
for i=1:K,
    aux=length(find(abs(CFM(i,:))>1e-7));
    if (max(Maux(i,:))>B2) | (aux<=B1),
        [v,ps]=max(er); er(ps(1))=0;
        Di(:,i)=PAM(:,ps(1))/norm(PAM(:,ps(1)));
        Maux=Di'*Di;Maux=Maux-diag(diag(Maux));
    end;
end;

disp('dictionary ready')

%Image denoising---------------------------------------------
Fout=zeros(fc,fr); %prepare space for denoised image
wgt=zeros(fc,fr); %weights
bks=im2col(Fn,[8,8],'sliding');
nub=size(bks,2); %number of blocks
ix=[1:nub];

disp('compute coefficients for denoising')
%Proceed with sets of 25000 coefficients
for nj=1:25000:nub,
    jsz=min(nj+25000-1,nub); %jump size
    cf= spc(Di,bks(:,nj:jsz),etg); %coefficients (sparse coding)
    bks(:,nj:jsz)=Di*cf;
    disp(['subset: ',num2str(nj),'-',num2str(jsz)]);
end;

disp('start denoising');
nn=1;
[r,c]=ind2sub([fc,fr]-7,ix);
for j=1:length(c),
    ic=c(j); ir=r(j);
    bk=reshape(bks(:,nn),[8,8]); %a block
    Fout(ir:ir+7,ic:ic+7)=Fout(ir:ir+7,ic:ic+7)+bk;
    wgt(ir:ir+7,ic:ic+7)=wgt(ir:ir+7,ic:ic+7)+ones(8);
    nn=nn+1;
end;

%combine with noisy image
Fd = (Fn+0.034*sig*Fout)./(1+0.034*sig*wgt); 

%Result-------------------------------------------------------
disp('result display');
figure(1)
subplot(1,2,1)
imshow(F0,[]);
title('original picture')
subplot(1,2,2)
imshow(Fn,[]);
title('noisy image')

figure(2)
imshow(Fd,[])
title('denoised image')

figure(3)
cD=zeros(9,9,1,K); %collection of Dictionary patches
for np=1:K,
    ni=1;
    for j=1:8,
        for i=1:8,
          cD(i,j,1,np)=Di(ni,np);
          ni=ni+1;
        end;
        cD(9,j,1,np)=1; %white line
    end;
    %patch contrast augmentation
    xx=1:8;
    cD(xx,xx,1,np)=cD(xx,xx,1,np)-min(min(cD(xx,xx,1,np)));
    aux=max(max(cD(xx,xx,1,np)));
    if aux>0, 
        cD(xx,xx,1,np)=cD(xx,xx,1,np)./aux;
    end;    
end;

montage(cD);
title('patch dictionary');
