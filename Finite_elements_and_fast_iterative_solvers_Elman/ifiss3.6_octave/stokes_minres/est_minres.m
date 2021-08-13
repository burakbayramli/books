function [x,resvec,infsup,errpde,info] = ...
                   est_minres(A,B,stabC,frhs,grhs,apost,stopit,maxit,tol,...
                              prob_type,MA,aparams,MQ,qparams,Mest,eparams);
% MINRES with discretization error estimation
% [x,resvec,infsup,errpde,info]= ...
%        est_minres(A,B,stabC,frhs,grhs,apost,stopit,maxit,tol,...
%                   prob_type,MA,aparams,MQ,qparams,Mest,eparams);
%   input
%         A,B,stabC       coefficient matrix [A,B;B',-stabC]
%         frhs,grhs       RHS vector [frhs;grhs]
%         apost           1/0 error estimation on/off  
%         stopit          1/0 stopping test on/off
%         maxit           max number of iterations
%         tol             stopping tolerance
%         prob_type       problem  type switch
%         MA,aparams      primal preconditioner (function_name, structure)
%         MQ,qparams      dual preconditioner 
%         Mest,eparams    error estimator 
%   output
%         x            iterative solution
%         resvec       vector of redidual errors
%         infsup       vector of infsup constant estimates
%         errpde       vector of  discretization error estimates
%         info         computed eigenvalue estimates
% calls function param_est
%   IFISS function: DJS; 2 January 2011.
% Copyright (c) 2010 D.J. Silvester, V. Simoncini
global amg_grid amg_smoother number_vcycles
if apost,
    fprintf('Call to EST_MINRES with built in error control ...\n')
    Ahandle=str2func(MA);  Qhandle=str2func(MQ); Ehandle=str2func(Mest);
    if stopit,
       fprintf('\n   k    Estimated-Error   Algebraic-Bound   Residual-Error') 
    else, fprintf('\n   k    Estimated-Error   Residual-Error'), end
else 
    fprintf('Call to EST_MINRES without error control ...\n')
    Ahandle=str2func(MA);  Qhandle=str2func(MQ);
end  
rhs=[frhs;grhs];x0=zeros(size(rhs));
na=size(A,1); nb=size(B,2);
M=[A,B;B',-stabC];
n=size(M,1);
%
infsup=[nan,nan,nan]; lambda=[nan,nan,nan];
b=rhs;
x = x0;
res0=b-M*x; 
res=zeros(n,1); 
res(1:na)=Ahandle(res0(1:na),aparams);  
res(na+1:n)=Qhandle(res0(na+1:n),qparams);   
arr=sqrt( res0'*res); 
erro=arr;
resvec(1)=arr; errpde(1)=nan;

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
   i=0; tstart=cputime;
   
   while (i<maxit & erro>tol) %----------- start of iteration loop
     i=i+1;
     i1=i+1;
% Matrix-vector product
     w1 = M*v;
     p1=v;
     if (i>1)
         H(i-1,i)=H(i,i-1);
         w1 = w1 - w0*H(i-1,i);
     end
     H(i,i)=v'*w1;
     w1 = w1 - w*H(i,i);

% Apply left precond P using P inner product for V.
     v(1:na,1)=Ahandle(w1(1:na),aparams);
     v(na+1:n,1)=Qhandle(w1(na+1:n),qparams);
     H(i+1,i)=sqrt(v'*w1);
     if (H(i+1,i) ~= 0.)
        v=v/H(i+1,i);     w1=w1/H(i+1,i);
     else,return,end;
%
    if i>2;
%   estimate the inf-sup constant from the harmonic ritz values
    [coef,delta,lambda_minus,emin] = param_est(H,i,prob_type);
    infsup=[infsup,emin]; lambda=[lambda,delta];
    end

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
    
     
    if apost,
%      estimate the approximation error
       errorest = Ehandle(x,eparams);
       fprintf('\n %3i    %10.4e  ',i,errorest)
%      use dynamic stopping tolerance 
       if stopit & i>5, tol=errorest/coef; 
       fprintf('      %10.4e  ',erro*coef); 
       elseif stopit & i<=5, fprintf('                  '), end
       fprintf('      %10.4e  ',erro);
       errpde=[errpde,errorest];
    end
   end   %------------- end of iteration loop

etoc=cputime-tstart;
fprintf('Bingo!\n')
fprintf('\nconvergence in %3i iterations\n',i)
fprintf('  %9.4e seconds\n\n',etoc)
info=[infsup(i),lambda(i)];  %% returns eigenvalue estimates via info
if apost,
   fprintf('\n Eigenvalue convergence')
   fprintf('\n    k     infsup     lambda \n')
      for its=4:i+1,
      fprintf('%5i %11.4f %11.4f \n', its-1, infsup(its-1), lambda(its-1));
      end  
   upperbd=[resvec*coef]'; kk=find(upperbd<errorest,1,'first');
   fprintf('\n')
   fprintf('\n Final estimated error is %10.4e',errorest)
   fprintf('\n Optimality in %3i iterations \n',kk-1)
   info=[info,errorest,i+1];
else
   errpde=[];
   info=[info,nan,i+1];
end
return