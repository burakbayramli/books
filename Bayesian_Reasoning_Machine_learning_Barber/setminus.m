function c=setminus(a,b)
% C=SETMINUS(A,B)
% C is the set A, without the elements B. C preserves the ordering of A
[dummy v]=setdiff(a,b);
c=a(sort(v));


