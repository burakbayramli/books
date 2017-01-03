echo on
clc

% This code gives the output in terms of FRACTIONS instead of decimals.
% This is only a display! You cannot assign a name B = rats(A).
%
% A fraction is a "rational number" = RATio of integers.
% At some point the exact fraction is not convenient and a nearby fraction 
% (or zero) is a better answer. Test to find that changeover point:

rats(.3333)
rats(.333333)
rats(.33333333)
1/3
rats(1/3)
% Question: What range of numbers will produce rats(x) = 1?
%  
% press any key
pause
clc 
% Rats is usually applied to all the entries in a vector or a matrix.
% If B is a matrix whose entries are fractions, so is its inverse. Reason...??

% Elimination gives an explanation; only fractions appear. The determinant
% formula for the inverse gives another reason; all cofactors are fractions.
% Does the reduced echelon form R also consist of fractions?

% The code inverse(B) does NOT produce fractions:
B = hilb(4); INV = inverse(B)
f = get(0,'format');
format long
NEAREYE = B*INV
format(f);
rats(NEAREYE)
% press any key
pause
clc
% The 4 by 4 Hilbert matrix has H(i,j) = 1/i + j - 1. It has various "inverses":
H = hilb(4)
M = inverse(H)
rats(M)
N = invhilb(4)
rats(N)
% How can M be different from N? The answer is roundoff error. It is studied
% in Chapter 9. Which has the error? 
%  
% Experiment: Increase the order n from 4 and compare inverses. How quickly
% does  inverse(hilb(n)) - invhilb(n) grow with n ?
echo off
