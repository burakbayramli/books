function [k,mu,M,match,aUpdate,rUpdate] = radialUpdate(match,aUpdate,rUpdate,k,mu,M,delta,x,y,hyper,t,bFunction,walkInt,walk);
% PURPOSE : Performs the update move of the reversible jump MCMC algorithm.
% INPUTS  : - match: Number of times a basis function already exists (probability zero in theory).
%             For completeness sake, I don't allow for duplicate basis functions here.
%           - aUpdate: Number of times the update move has been accepted.
%           - rUpdate: Number of times the update move has been rejected.
%           - k : Number of basis functions.
%           - mu : Basis functions centres.
%           - M : Regressors matrix.
%           - delta : Signal to noise ratio.
%           - x : Input data.
%           - y : Target data.
%           - hyper: hyperparameters.
%           - t : Current time step.
%           - bFunction: Type of basis function.
%           - walk, walkInt: Parameters defining the compact set from which mu is sampled.

% AUTHOR  : Nando de Freitas - Thanks for the acknowledgement :-)
% DATE    : 21-01-99

if nargin < 14, error('Not enough input arguments.'); end
[N,d] = size(x);      % N = number of data, d = dimension of x.
[N,c] = size(y);      % c = dimension of y, i.e. number of outputs.
insideUpdate=1;
uU=rand(1);

% INITIALISE H AND P MATRICES:
% ===========================
invH=zeros(k(t)+1+d,k(t)+1+d,c);
P=zeros(N,N,c);
invHproposal=zeros(k(t)+1+d,k(t)+1+d,c);
Pproposal=zeros(N,N,c);
proposal=zeros(1,d);

% UPDATE EACH CENTRE:
% ==================   
if k(t)==0
  mu{t+1}=mu{t};   
end;
for basis=1:k(t),
  for i=1:c,
    invH(:,:,i) = (M'*M + (1/delta(t,i))*eye(k(t)+1+d));
    P(:,:,i) = eye(N) - M*inv(invH(:,:,i))*M';
  end;

  % PROPOSE A BASIS FUNCTION TO BE UPDATED:
  % ======================================
  for i=1:d,
    proposal(1,i)= (min(x(:,i))-walk(i)) + ((max(x(:,i))+walk(i))-(min(x(:,i))-walk(i)))*rand(1,1);
  end;

  % CHECK IF THE PROPOSED CENTRE ALREADY EXISTS:
  % =========================================== 
  match1=0;
  notEnded=1;
  i=1;
  while ((match1==0)&(notEnded==1)),
    if (mu{t}(i,:)==proposal),
      match1=1;
    elseif (i<k(t)),
      i=i+1;
    else
      notEnded=0;
    end;
  end;
  match2=0;
  notEnded=1;
  i=1;
  if basis>1,
    match2=0;
    notEnded=1;
    i=1;
    while ((match2==0)&(notEnded==1)),
      if (mu{t+1}(i,:)==proposal),
        match2=1;
      elseif (i<basis-1),
        i=i+1;
      else
        notEnded=0;
      end;
    end;
  end; 
  if (match1>0),
    match=match+1;
    mu{t+1}(basis,:)=mu{t}(basis,:);
  elseif (match2>0),
    match=match+1;
    mu{t+1}(basis,:)=mu{t}(basis,:);
  else
    % IF IT DOESN'T EXIST, PERFORM AN UPDATE MOVE:
    % ===========================================
    Mproposal = M;
    Mproposal(:,d+1+basis) = feval(bFunction,proposal,x);
    for i=1:c,
      invHproposal(:,:,i) = (Mproposal'*Mproposal + inv(delta(t,i))*eye(k(t)+1+d));
      Pproposal(:,:,i) = eye(N) - Mproposal*inv(invHproposal(:,:,i))*Mproposal'; 
    end;
    ratio = 1;
    for i=1:c,
     ratio = ratio*sqrt((det(invH(:,:,i)))/(det(invHproposal(:,:,i))))*(((hyper.gamma+y(:,i)'*P(:,:,i)*y(:,i))/(hyper.gamma+y(:,i)'*Pproposal(:,:,i)*y(:,i)))^((hyper.v+N)/2)); 
    end;
    acceptance = min(1,ratio);
    if (uU<acceptance),
      mu{t+1}(basis,:) = proposal;
      M=Mproposal;
      aUpdate=aUpdate+1;
    else
      mu{t+1}(basis,:) = mu{t}(basis,:);
      rUpdate=rUpdate+1;
      M=M;
    end;
  end;
end;
k(t+1) = k(t); % Don't change dimension.
M = M;         % Return the last value of M.











