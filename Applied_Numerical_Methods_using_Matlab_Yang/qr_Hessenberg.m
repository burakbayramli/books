function [Q,R]=qr_Hessenberg(Hs)
%QR factorization of Hessenberg form by Givens rotation
N=size(Hs,1);
Q=eye(N); R=Hs;
for k=1:N-1
   x=R(k,k); y=R(k+1,k); r=sqrt(x*x+y*y);
   c=x/r; s=-y/r;
   R0=R; Q0=Q;
   R(k,:)=c*R0(k,:)-s*R0(k+1,:); 
   R(k+1,:)=s*R0(k,:)+c*R0(k+1,:); 
   Q(:,k)=c*Q0(:,k)-s*Q0(:,k+1);
   Q(:,k+1)=s*Q0(:,k)+c*Q0(:,k+1);
end
