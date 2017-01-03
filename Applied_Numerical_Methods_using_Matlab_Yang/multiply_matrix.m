function C=multiply_matrix(A,B)
[M,K]=size(A);  [K1,N]=size(B);
if K1~=K
  error('The # of columns of A is not equal to the # of rows of B')
else   
  for m=1:
     for n=1:
        C(m,n)=A(m,1)*B(1,n);
        for k=2:
           C(m,n)=C(m,n)+A(m,k)*B(k,n);
        end         
     end
  end
end  
