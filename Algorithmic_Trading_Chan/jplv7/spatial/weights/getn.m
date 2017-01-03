function y=f(n,t)

% autor Johann Caro Burnett
% obtiene el valor número n de una variable de caracteres t.
% ej. t=123 34545 65 sdf 565 erf3 23
% getnm(3,t)
%          =65

ws=isspace(t);
[a b]=size(t);
ws=ws+zeros(1,b);
i=1;
p=1;

while p<n
    
    if ws(i)==1
    p=p+1;
    end
    i=i+1;
    
end

y=0;


while ws(i)==0
    
    y=10*y+(t(i)-48);
    i=i+1;
    
    
end