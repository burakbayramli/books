% Example of Vector Fitting
clear all
Ns=101; %number of frequency samples
s=2*pi*j*logspace(0,4,Ns); %set of frequency values
H=zeros(1,Ns); %space for frequency responses
offs=2; 

% Fequency response of a given plant (edit)
for k=1:Ns,
    sk=s(k);
    H1=2/(sk+5); 
    H2=(30+j*40)/(sk-(-100+j*500)); 
    H3=(30-j*40)/(sk-(-100-j*500)); 
    H(1,k)=H1+H2+H3+0.5;
end; 

%xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
% Start the algorithm

% Initial poles
N=3; %number of poles
pa=-2*pi*logspace(0,4,N); %initial poles
Lam=diag(pa);

%=============================================================
% Identification of poles

% Matrix building-------------------------------
% Cx=0, real pole; 
% Cx=1, real part of complex pole; Cx=2, imag part of complex pole
Cx=zeros(1,N); %pole labels
Cx(1)=0; Cx(2)=0; Cx(3)=0; %initial poles are real

% factors
Dk=zeros(Ns,N);
Dk(:,1)=1./(s-Lam(1,1)); Dk(:,2)=1./(s-Lam(2,2)); Dk(:,3)=1./(s-Lam(3,3));
Dk(:,4)=1; Dk(:,5)=s;
   
% scaling
aux=norm(H)^2;
scl=sqrt(aux)/Ns;

% combined matrix
M=N+1; No=N+offs;
AA=zeros(M,M); bb=zeros(M,1); Escl=zeros(1,M);
A=zeros(Ns,No+M);
%left part
for m=1:No,
    A(1:Ns,m)=Dk(1:Ns,m);
end
%right part
for m=1:M,
    A(1:Ns,No+m)=-Dk(1:Ns,m).*H.';
end

A=[real(A);imag(A)];

%Integral criterion   
for m=1:N+1  
 A(2*Ns+1,No+m)=real(scl*sum(Dk(:,m)));
end

[Q,R]=qr(A,0);  %QR decomposition
ix1=No+1;
ix2=No+M;
R22=R(ix1:ix2,ix1:ix2);
AA(1:M,:)=R22;    
bb(1:M,1)=Q(end,No+1:end)'*Ns*scl; 
  
for col=1:M      
  Escl(col)=1/norm(AA(:,col));  
  AA(:,col)=Escl(col).*AA(:,col);
end  
x=AA\bb;  
x=x.*Escl.';

C=x(1:end-1);
D=x(end);
  
% Zeros of sigma---------------------------
B=ones(N,1);
Zer=Lam-B*C.'/D;
rt=eig(Zer).';
% take care of unstable roots
unst=real(rt)>0;
rt(unst)=rt(unst)-2*real(rt(unst)); %force stable roots

rt=sort(rt);
N=length(rt);
% sorting
for n=1:N
  for m=n+1:N
    if imag(rt(m))==0 && imag(rt(n))~=0
      aux=rt(n); rt(n)=rt(m); rt(m)=aux;
    end
  end
end

Nr=0; %number of real roots
for m=1:N
  if imag(rt(m))==0, Nr=m; end
end
if Nr<N, rt(Nr+1:N)=sort(rt(Nr+1:N)); end  

rt=rt-2*j*imag(rt); 
SERA=rt.';

%=============================================================
% Identification of residues

% Use of the zeros (rt) as new poles ----------------------
Lam=rt;

% pole labelling
Cx=zeros(1,N);
for m=1:N,
 if imag(Lam(m))~=0  
  if m==1, Cx(m)=1; end;
  if m~=1,
   if (Cx(m-1)==0 || Cx(m-1)==2) 
       Cx(m)=1;Cx(m+1)=2; 
   else
       Cx(m)=2; 
   end
  end
 end 
end

% Matrix building-------------------------------
A=zeros(2*Ns,N+2); BB=zeros(2*Ns,1);
wg=ones(1,Ns);

% factors
Dk=zeros(Ns,N);
for m=1:N
  if Cx(m)==0      %real pole
    Dk(:,m)=1./(s-Lam(m));
  elseif Cx(m)==1  %complex pole, 1st part
    Dk(:,m)  =1./(s-Lam(m)) + 1./(s-Lam(m)');
    Dk(:,m+1)=j./(s-Lam(m)) - j./(s-Lam(m)');
  end
end  

% combined matrix
A(1:Ns,1:N)=Dk; A(1:Ns,N+1)=wg; A(1:Ns,N+2)=wg.*s; 
BB(1:Ns)=H;
A(Ns+1:2*Ns,:)=imag(A(1:Ns,:));
A(1:Ns,:)=real(A(1:Ns,:));
BB(Ns+1:2*Ns)=imag(BB(1:Ns));
BB(1:Ns)=real(BB(1:Ns));

% re-scale
aux=length(A(1,:));
Escl=zeros(1,aux);
for col=1:aux
    Escl(col)=norm(A(:,col),2);
    A(:,col)=A(:,col)./Escl(col);
end
X=A\BB;
X=X./Escl.';
X=X.';
C=X(1:N);
SERE=X(N+2); 
SERD=X(N+1);

% get back a complex C
for m=1:N
 if Cx(m)==1    
   r1=C(m); r2=C(m+1);
   C(m)=r1+j*r2; C(m+1)=r1-j*r2;  
 end
end

%==============================================
% Final fitting
Hf=zeros(Ns); %reserve space for fitted data
Dk=zeros(Ns,N);
for m=1:N
    Dk(:,m)=1./(s-Lam(m));
end

Hf=(Dk*C.').';
Hf=Hf+SERD+SERE*s;
%==============================================
% Display
freq=s./(2*pi*j);

figure(3)
subplot(2,1,1)
 loglog(freq,abs(H),'kx'); hold on;
 loglog(freq,abs(Hf),'r');
 axis([1e0 1e4 0.3e0 1e0]);
 title('Freq. response data (x) vs fitting (continuous)') 
 xlabel('freq.(Hz)'); ylabel('log|H|'); 
subplot(2,1,2)
 semilogx(freq,angle(H),'kx'); hold on;
 semilogx(freq,angle(Hf),'r');
 xlabel('freq.(Hz)'); ylabel('angle(H)');


