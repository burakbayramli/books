% MCA example
% The signal is a mix of 2 sines and 3 random spikes

% signal synthesis ----------------------------
N=1024; %length
% sines have almost equal frequencies:
A=0.6; %sine amplitude
itv=1;
t=0:itv:N-itv;
s1=A*cos(pi*0.0307*t); 
s2=A*cos(pi*0.0309*t); 
ys=s1+s2; %sum of sines
% 3 spikes at random positions:
k=3;
v=[randn(k,1); zeros(N-k,1)]; p=randperm(N);
yk=v(p); pos=find(yk);
miny=min(yk); maxy=max(yk);
yk(pos) = (yk(pos)-miny)/(maxy-miny)+1; %normalize
% the composite signal
y = ys + yk';
signal=y(:); %column format
maxp=max(signal); %to set vertical limit in figures
nfg=0; %figure interval counter

% MCA parameters
qq=4; %to specify how fine is the approx
itermax 	= 200;
ndict=2; %number of dictionaries

%-----------------------------------------
    
%Initial variables  
     part= zeros(N,ndict);

%Initial sigma-------------------------------------------
    [Wc,Wl]=wavedec(signal,10,'db4'); %wavelet toolbox
    L=sum(Wl(1:10)); %to select details
    Dv=-Wc(L+1:end);
    % MAD method
    aux=Dv(find(~isnan(Dv)));
    sigma = median(abs(aux - median(aux)))./0.6745;
      
%Initial coeffs (using two dictionaries: DCT and Dirac)    
    coeff={};
    % DCT coeffs:
    nv=N*qq; fq=(1:(nv-1))';
    Ko=[N; 0.5*(N+sin(2*pi*fq/qq)./(2*sin(pi*fq/nv)))];
    Ko=Ko.^0.5;
    x=zeros(4*nv,1); L=2*N;
    x(2:2:L)=signal(:);
    z=fft(x);    
    coeff{1}= [struct('coeff',[]) struct('coeff',real(z(1:nv))./Ko)];
    % Dirac coeffs:   
    coeff{2}= [struct('coeff',[]) struct('coeff', signal(:))];

%Initial threshold (minimum of maximal coeffs in each dictionary)    
    for nn=1:ndict
      aux = [];  cfs = coeff{nn};
      sinx = length(cfs);
      for j = 2:sinx
  	    aux = [aux;cfs(j).coeff(:)];
      end
      buf(nn)=max(abs(aux(:)));
    end
    buf=flipud(sort(buf(:),1))';
    deltamax=buf(2);
   
	delta=deltamax;
    lambda=delta/(itermax-1); %Linear decrease: slope
    
%Start the algorithm---------------------------

for iter=0:itermax-1
    %residual computation
    residual=signal-sum(part,2);
	 
   % DCT part-------------
    Ra=part(:,1)+residual;
    %analysis:
    x=zeros(4*nv,1); L=2*N;
    x(2:2:L)=Ra(:); z=fft(x);    
    Ca= [struct('coeff',[]) struct('coeff',real(z(1:nv))./Ko)];
    %thresholding (not the low frequency components): 
    cf = Ca; ay=cf(2).coeff; 
    cf(2).coeff=ay.*(abs(ay)>delta);
 	Ca = cf;
    
    %synthesis:
    c=Ca(2).coeff; lc=length(c); M=lc/qq;
    fu=(1:(lc-1))';
    Ku=[M; 0.5*(M+sin(2*pi*fu/qq)./(2*sin(pi*fu/lc)))];
    Ku=Ku.^0.5;
    c=c./Ku;
    x=zeros(4*lc,1); L=2*M;
    x(1:lc)=c; z=fft(x);
    y=real(z(2:2:L));
    part(:,1)=y(:)/qq; %output
    
   % Dirac part-------------- 
    Ra=part(:,2)+residual;
    %analysis:
    Ca= [struct('coeff',[]) struct('coeff', Ra(:))];
    %thresholding (not the low frequency components): 
    cf = Ca; ay=cf(2).coeff; 
    cf(2).coeff=ay.*(abs(ay)>delta);
 	Ca = cf;
      
    %synthesis:
    part(:,2)=Ca(2).coeff(:); %output    
  
    % Update parameters---------------
	delta=delta-lambda; %linear decrease  
    
    % Display along the process
    nfg=nfg+1;
    if nfg==4, 
      nfg=0; %restart counter
    figure(1)
     subplot(3,1,1)
      plot(sum(part(1:N,:),2));axis tight;drawnow;
      title('sum of detected parts')
      axis([0 N -maxp maxp]);
     subplot(3,1,2)
      plot(part(1:N,1));axis tight;drawnow;
      title('the sum of 2 sines')
      axis([0 N -maxp maxp]);
     subplot(3,1,3)
      plot(part(1:N,2));axis tight;drawnow;
      title('the 3 spikes at random')
      axis([0 N 0 maxp]);
    end 
      
end
part = part(1:N,:);

% Final display-----------------------------------
%Original signals
     figure(2)
     subplot(3,1,1)
      plot(signal);axis tight;
      title('the original composite signal')
      axis([0 N -maxp maxp]);
     subplot(3,1,2)
      plot(ys);axis tight;
      title('the original sum of 2 sines')
      axis([0 N -maxp maxp]);
     subplot(3,1,3)
      plot(yk);axis tight;
      title('the original 3 spikes at random')
      axis([0 N 0 maxp]);





