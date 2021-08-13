function [x,resvec,iter,flag] = poissoncontrol_minres_reduced(M,K,beta, ...
    Myhat,d,maxit,tol,MA,aparams,MS,sparams)
%POISSONCONTROL_MINRES_REDUCED MINRES for (reduced) 2x2 block system
% [x,resvec,iter,flag] = poissoncontrol_minres_reduced(M,K,beta, ...
%     Myhat,d,maxit,tol,MA,aparams,MS,sparams)
%   input
%         M,K,beta        coefficient matrix [M,K;K,-1/beta*M]
%         Myhat,d         RHS vector [Myhat;d]
%         maxit           max number of iterations
%         tol             stopping tolerance
%         MA,aparams      preconditioner for (1,1)-block
%                         (function name & structure)
%         MS,sparams      preconditioner for Schur complement
%   output
%         x            iterative solution
%         resvec       vector of residual errors
%         iter         number of MINRES iterations taken
%         flag         indicates success (0) or failure (1) of method
%   IFISS function: JWP, DJS; 4 July 2012.
% Copyright (c) 2012 J.W. Pearson, D.J. Silvester, V. Simoncini
global amg_grid amg_smoother number_vcycles

fprintf('Call to POISSONCONTROL_MINRES_REDUCED ...\n')
Ahandle=str2func(MA);  Shandle=str2func(MS);

nm=size(M,1);
rhs=[Myhat;d];x0=zeros(size(rhs));
Matrix=[M,K; K,-1/beta*M];
n=size(Matrix,1);
%
b=rhs;
x = x0;
res0=b-Matrix*x; 
res=zeros(n,1); 
res(1:nm)=Ahandle(res0(1:nm),aparams);  
res(nm+1:n)=Shandle(res0(nm+1:n),sparams);   
arr=sqrt( res0'*res); 
erro=arr;
resvec(1)=arr;

   R = zeros(4,1);

% Q contains rotations. c_k, s_k could be stored instead
   cc=zeros(2,1);ss=zeros(2,1);

% Lanczos vectors
   w0=zeros(n,1);

% Auxiliary vectors for updating solution
   p0=zeros(n,1); p=zeros(n,1);
   v = res/arr; 
   w = res0/arr;
   F=arr*speye(2,1);
   i=0; % tstart=cputime;
   H = [];
   
   while (i<maxit & erro>tol) %----------- start of iteration loop
     i=i+1;
     i1=i+1;
% Matrix-vector product
     w1 = Matrix*v;
     p1=v;
     if (i>1)
         H(i-1,i)=H(i,i-1);
         w1 = w1 - w0*H(i-1,i);
     end
     H(i,i)=v'*w1;
     w1 = w1 - w*H(i,i);

% Apply left precond P using P inner product for V.
     v(1:nm,1)=Ahandle(w1(1:nm),aparams);
     v(nm+1:n,1)=Shandle(w1(nm+1:n),sparams);
     H(i+1,i)=sqrt(v'*w1);
     if (H(i+1,i) ~= 0.)
        v=v/H(i+1,i);     w1=w1/H(i+1,i);
     else,return,end;

% Apply previous rotations
    if (i==1), R(3)=H(1,1); end
    if (i==2),
     R(2:3,1)=GG*[H(1:i,i)];
     GGold=GG(:,2);
    end
    if (i>2),
     R(1,1)=GGold(1)*H(i-1,i); R(2,1)=GGold(2)*H(i-1,i);
     R(2:3,1)=GG*[R(2);H(i,i)];
     GGold=GG(:,2);
    end

% Determine and apply new rotations
    nrm=sqrt(R(3)^2+H(i1,i)^2);
    GG(1,1) = abs(R(3))/nrm; GG(1,2)=R(3)/abs(R(3))*conj(H(i1,i))/nrm;
    GG(2,1) = -conj(GG(1,2)); GG(2,2)=GG(1,1);
    R(3)   = GG(1,:) *[R(3);H(i1,i)];
    F(1:2)     = GG *F(1:2);

% Update approx solution
    p1 = (p1 - p0*R(1)-p*R(2))/R(3);
    x  = x + p1*F(1);
    F(1:2)=[F(2);0];
    w0=w; w=w1;
    p0=p; p=p1;
    erro=full(abs(F(1)));
    resvec=[resvec,erro];
   end   %------------- end of iteration loop

if erro>tol
    flag = 1; iter = i;
    fprintf('iteration aborted! Iteration returned with flag equal to  %2i \n',flag)
else
    flag = 0; iter = i;
%     etoc=cputime-tstart;
%     fprintf('Sweet!\n')
%     fprintf('\nconvergence in %3i iterations\n',i)
%     fprintf('  %9.4e seconds\n\n',etoc)
end

return
