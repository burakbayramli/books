function [data, Ht] = full_bekk_simulate(t,k,parameters,p,q)
% PURPOSE:
%     To simulate a full BEKK multivariate GARCH model. 
% 
% USAGE:
%     [data, Ht] = full_bekk_simulate(t,k,parameters,p,q)
% 
% INPUTS:
%     t             - Length of data serie to prouce
%     k             - Dimension of series to produce
%     parameters    - A vecotr continaing C, A's, and B's.  Shoudlhave length (k*(k+1))/2 + p*k^2 + q*k^2
%     p             - The lag length of the innovation process
%     q             - The lag length of the AR process
% 
% OUTPUTS:
%     data          - A t by k matrix of zero mean residuals
%     Ht            - A k x k x t 3 dimension matrix of conditional covariances
% 
% 
% COMMENTS:
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

t=t+500;
k2=k*(k+1)/2;

%Reshape the parameters
C=ivech(parameters(1:(k*(k+1)/2))');
C=tril(C);

LHS=eye(k^2);

A=parameters(k2+1:k2+k*k*p);
B=parameters(k2+k*k*p+1:k2+k*k*p+k*k*q);
tempA=zeros(k,k,p);
tempB=zeros(k,k,p);
for i=1:p
    tempA(:,:,i)=reshape(A((k*k*(i-1)+1):(k*k*i)),k,k);
end
for i=1:q
    tempB(:,:,i)=reshape(B((k*k*(i-1)+1):(k*k*i)),k,k);
end
A=tempA;
B=tempB;

for i=1:p
    LHS=LHS-kron(A(:,:,i),A(:,:,i));
end

for i=1:q
    LHS=LHS-kron(B(:,:,i),B(:,:,i));
end

Ht=zeros(k,k,t);
%Calculate the unconditional covariance
Cvec=(C*C');
Cvec=Cvec(:);
U=LHS^(-1)*Cvec;
U=reshape(U,k,k);
data=randn(t,k)*U^(0.5);
m=max(p,q);
Ht(:,:,1:m)=U;

const=C*C';
for i=m+1:t+m;
    Ht(:,:,i)=const;
    for j=1:p
         Ht(:,:,i)=Ht(:,:,i)+A(:,:,j)*(data(i-j,:))'*(data(i-j,:))*A(:,:,j)';
    end
    for j=1:q
         Ht(:,:,i)=Ht(:,:,i)+B(:,:,j)*Ht(:,:,i-j)*B(:,:,j)';
    end
    data(i,:)=randn(1,k)*Ht(:,:,i)^(0.5);
end
data=data(m+500:t+m,:);
Ht=Ht(:,:,m+500:t+m);
