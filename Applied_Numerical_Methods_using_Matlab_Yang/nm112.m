%nm112
clear, clf
A=[1 2 3;4 5 6]
B=[3;-2;1];
C(2)=2; C(4)=4
disp('Press any key to see the input/output through Files')
save ABC A B C %save as a MAT-file
clear('A','C')
load ABC A C %read MAT-file
save B.dat B /ascii %save as an ASCII-file
clear('B')
load b.dat %read ASCII-file
b
x=input('Enter x:')
format short e
x
format rat
x
format long
x
format short
x

