function  x=gauss(A,B,pivoting)
%The sizes of given matrices A,B are supposed to be NAxNA and NAxNB, respectively.
%This function finds x=A^-1 B by Gauss-Jordan elimination algorithm.
%pivoting=2(default) for scaled partial pivoting/1(partial pivoting)/0(no row swapping)
if nargin<3, pivoting=2; end %scaled partial pivoting by default
[NA1,NA]=size(A);  [NB1,NB]=size(B);
if NA1~=NA|NB1~=NA, error('A and B must have compatible dimension'); end
N= NA+NB;
AB=[A(1:NA,1:NA) B(1:NA,1:NB)]; % Augmented matrix
epss=eps*ones(NA,1);
AB(NA+1,:)= [1:NA  zeros(1,NB)]; % the index set of variables
for k=1: NA
   switch pivoting
     case 2, [akx,kx]=max(abs(AB(k:NA,k))./max(abs([AB(k:NA,k+1:NA) epss(1:NA-k+1)]'))');  
     otherwise, [akx,kx]=max(abs(AB(k:NA,k)));  %partial pivoting
   end    
   if akx<eps  
      disp('Singular coefficient matrix and No unique solution')
      [akx,kx]= max(abs(AB(k,k+1:NA))); 
      if k<NA&akx>eps
        tmp_col= AB(:,kx); %try column change
        AB(:,kx)= AB(:,k);  AB(:,k)= tmp_col;
       else
        if abs(AB(k,NA+1))>eps, error('inconsistent case')
         else  x(AB(NA+1,1:NA))= AB(1:NA,NA+1); error('indetermined case')
        end          
      end 
   end
   mx =k+kx-1;  
   %Partial Pivoting at AB(k,k)
   if kx>1&pivoting>0 % Row change if necessary
      tmp_row =AB(k, k:N);
      AB(k, k:N) =AB(mx, k:N);
      AB(mx, k:N) =tmp_row;
   end
   % Gauss elimination
   AB(k, k+1:N) =AB(k, k+1:N)/AB(k,k);
   AB(k,k) =1;
   for m=k+1: NA
      AB(m, k+1:N) =AB(m, k+1:N)-AB(m, k)*AB(k, k+1:N);
      AB(m,k) =0;
   end
end
%backward substitution for a upper-triangular matrix eqation
% having all the diagonal elements equal to one
x(NA,:)= AB(NA,NA+1:N); 
for m=NA-1: -1:1
   x(m,:)= AB(m,NA+1:N)-AB(m,m+1:NA)*x(m+1:NA,:); %Eq.(2.2-7)
end
%x =backsubst(AB(1:NA,1:NA),AB(:,NA+1:N));