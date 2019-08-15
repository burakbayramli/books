function [p1,field1] = mesh04(p,pt,field);
% Eckart Gekeler, Universitaet Stuttgart, Release 15.1.06
% eliminiert doppelte Knoten in der Knotenmatrix pt
% und dem Feld der Knotennummern FIELD
field1 = field; 
L = size(p,2), p_aux = pt'; [M,N] = size(field);
[p_aux1,I,J] = unique(p_aux,'rows');
p1 = [p,p_aux1'];
J = [[1:L],L+J'];
C = field(:);
D = J(C);
field1 = reshape(D,M,N);
