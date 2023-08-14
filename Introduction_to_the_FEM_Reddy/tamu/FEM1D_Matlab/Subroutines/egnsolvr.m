function egnsolvr(n,a,b,xx,x,negn,nr,mxneq)
% c     __________________________________________________________________
% c
% c      the subroutine is called in main to solve the eigenvalue problem
% c
% c                            [a]{x} = lambda.[b]{x}
% c
% c       the program can be used only for positive-definite [b] matrix.
% c       the dimensions of v, vt, w, and ih should be equal to mxneq.
% c     __________________________________________________________________
% c
%       implicit real*8 (a-h,o-z)
%       dimension  a(mxneq,mxneq),b(mxneq,mxneq),xx(mxneq),x(mxneq,mxneq)
%       dimension  v(500,500),vt(500,500),w(500,500),ih(500)
% c
% c     call subroutine jacobi to diagonalize [b]
% c

%call jacobi (n,b,negn,nr,v,xx,ih,mxneq)
b
jacobimethod(n,b,negn,v,nr)
b
stop
%c make diagonalized [b] symmetric

for i=1:n
    for j=1:n
        b(j,i)=b(i,j);
    end
end
%c     check (to make sure) that [b] is positive-definite
% for 30 i=1,n
%     if (b(i,i))20,30,30
%         20 write(6,80)
%         stop
%     end
% end
%c     the eigenvectors of [b] are stored in array v(i,j)
%c     form the transpose of [v] as [vt]
for i=1:n
    for j=1:n
        vt(i,j)=v(j,i);
    end
end
%c find the product [f]=[vt][a][v] and store in [a] to save storage
% call matrxmlt (mxneq,n,vt,a,w)
% call matrxmlt (mxneq,n,w,v,a)
%c get [gi] from diagonalized [b], but store it in [b]
for i=1:n
    b(i,i)=1.0/dsqrt(b(i,i));
end
%c find the product [q]=[gi][f][gi]=[b][a][b] and store in [a]
% call matrxmlt (mxneq,n,b,a,w)
% call matrxmlt (mxneq,n,w,b,a)
%c we now have the form [q]{z}=lamda{z}. diagonalize [q] to obtain
%c  the eigenvalues by calling jacobi.
% call jacobi (n,a,negn,nr,vt,xx,ih,mxneq)
%c     the eigenvalues are returned as diag [a].
for j=1:n
    xx(j)=a(j,j);
end
% c  the eigenvectors are computed from the relation,
% c  {x}=[v][gi]{z}=[v][b][vt]
% c  since {z} is stored in [vt].
% call matrxmlt (mxneq,n,v,b,w)
% call matrxmlt (mxneq,n,w,vt,x)
% 80 format(/'*** matrix [glm] is not positive-definite ***')
end

