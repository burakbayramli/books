%do_condition.m
clear
for m=1:6
   for n=1:6
      A(m,n) =1/(m+n-1);
   end
end
A
for N=7: 12   
   for m=1:N,   A(m,N) =1/(m+N-1);  end
   for n=1:N-1,   A(N,n) =1/(N+n-1);  end
   c =cond(A);  d=det(A)*det(A^-1);
   fprintf('N=%2d: cond(A)=%e,  det(A)det(A^-1)=%8.6f\n', N, c, d);
   if N==10,  AAI=A*A^-1, end
end

