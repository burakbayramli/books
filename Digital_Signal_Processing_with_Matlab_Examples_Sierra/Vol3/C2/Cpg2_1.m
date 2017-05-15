% LARS/LASSO example
% diabetes data

% data matrix (the last column is the response)

data=load('diabetes_table.txt');

% Data preprocessing: 
% zero mean, unit L2 norm, response with no bias
[ll,cc]=size(data); A=zeros(ll,cc-1); b=zeros(ll,1);
for j=1:cc-1,
    A(:,j)=data(:,j)-mean(data(:,j)); %mean removal
    aux=sqrt(A(:,j)'*A(:,j)); A(:,j)=A(:,j)/aux; %unit L2-norm
end;
b=data(:,cc)-mean(data(:,cc));

% Initialization of variables
  hbeta=[]; 
  maxnav=min(ll,cc-1); %maximum number of active variables
  gamma_av=0;
  %valid sign flag:
  vls=1; %valid

% LARS/LASSO: ------------------------------------------------
lasso_f=0; % flag (0- LARS; 1-LASSO) (Edit here)
ni=1; nitmax=3600;
for t=0:100:nitmax, 
 
  %Initialization of variables  
  beta=zeros(cc-1,1);
  mu=zeros(ll,1); 
  c=[]; maxc=0; maxc_ix=0;
  J=[]; %indices of the active set
  signJ=[];
  %active set variables (av):
  nav=0; %counter of active variables
  gamma_av=0;
  %valid sign flag:
  vls=1; %valid
  inz=1; %initialize flag
  bkloop=0; %for loop breaking
  norm1=0;
  oldnorm1=0;

    while nav<maxnav,
        c=A'*(b-mu); %current corr
        maxc=max(abs(c));
        if inz==1, [aux, maxc_ix]=max(abs(c)); inz=0; end; %initialization
        if vls==1, J=[J, maxc_ix]; nav=nav+1; end; %add index to J
        
        signJ=sign(c(J)); 
        complJ=setdiff(1:maxnav,J); %complement of J
        LcomplJ=length(complJ);
        
        Aj=A(:,J);
        for k=1:length(J),
            Aj(:,k)=signJ(k)*Aj(:,k);
        end;
        G=Aj'*Aj;          
        Ij=ones(length(J),1);
        iG=inv(G);
        Q=1/sqrt(Ij'*iG*Ij);
        wj=Q*iG*Ij;
        uj=Aj*wj;      
        a=A'*uj;
        if nav==maxnav, gamma_av =maxc/Q; %unusual last stage
        else
          gamma=zeros(LcomplJ,2); %min of two terms
          for k=1:LcomplJ,
              n=complJ(k);
              gamma(n,:)=[((maxc-c(n))/(Q-a(n))),((maxc+c(n))/(Q+a(n)))];
          end;
          
          %remove complex elements, reset to Inf
          [pi,pj]=find(0~=imag(gamma)); 
          for nn=1:length(pi), gamma(pi(nn),pj(nn))=Inf; end;
          % find minimum
          gamma(gamma<=0) = Inf;
          mm=min(min(gamma)); gamma_av=mm;
          [maxc_ix,aux]=find(gamma==mm);   
          
        end;
     
        baux(J)=beta(J)+gamma_av*diag(signJ)*wj; %updating coeff estimate
        bkloop=0;
        Jold=J;
        
        % The LASSO option ------------
        if lasso_f==1, 
            vls=1; %valid sign
            dh=diag(signJ'*wj);
            gamma_c = -beta(J)./dh;
                       
             %remove complex elements, reset to Inf
             [pi,pj]=find(0~=imag(gamma_c)); 
             for nn=1:length(pi), gamma_c(pi(nn),pj(nn))=Inf; end;
             % find minimum
             gamma_c(gamma_c <= 0) = Inf;
             mm=min(min(gamma_c)); gamma_w=mm;
             [gamma_w_ix,aux]=find(gamma_c==mm);   
          
            if isnan(gamma_w), gamma_w=Inf; end; %Lasso modification
            if gamma_w < gamma_av,
                gamma_av = gamma_w;
                baux(J)=beta(J)+gamma_av*diag(signJ)*wj;
                J(gamma_w_ix)=[]; %delete zero-crossing element
                nav=nav-1;
                vls=0;
            end;
        end;
        % ------------------------------
                
        norm1=norm(baux(Jold),1);
        if oldnorm1<=t && norm1>=t,           
            baux(Jold)=beta(Jold)+Q*(t-oldnorm1)*diag(signJ)*wj;
            bkloop=1; %for loop break            
        end;
        oldnorm1=norm1;        
        mu=mu+ gamma_av*uj; 
        beta(Jold)=baux(Jold);
        if bkloop==1, break; end %while end
    end; 
        
    hbeta(ni,:)=beta';
    ni=ni+1;
end;

% display ------------------------------------------------
[bm,bn]=size(hbeta);
aux=[0:100:nitmax];
q=0; %color switch

figure(1)
hold on; q=0;
for np=1:bn,    
    if q==0,
        plot(aux,hbeta(:,np),'r',aux,hbeta(:,np),'k.');
        axis([0 nitmax -1000 1000]);
        if lasso_f==0,
          title('LARS, diabetes set');
        else
          title('LASSO, diabetes set'); 
        end;
    end;
    if q==1,
        plot(aux,hbeta(:,np),'b',aux,hbeta(:,np),'m.');
        axis([0 nitmax -1000 1000]);  end    
    if q==2,
        plot(aux,hbeta(:,np),'k',aux,hbeta(:,np),'b.');
        axis([0 nitmax -1000 1000]);  end    
    q=q+1; if q==3, q=0; end;
end;
grid;
  