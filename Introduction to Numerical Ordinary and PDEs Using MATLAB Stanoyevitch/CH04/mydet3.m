function y = mydet3(A)
y=A(1,1)*mydet2(A(2:3,2:3))-A(1,2)*mydet2(A(2:3,[1 3]))+A(1,3)*mydet2(A(2:3,1:2));