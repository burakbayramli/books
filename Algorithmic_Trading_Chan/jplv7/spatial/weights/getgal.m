function [N W]=fun(filename)

% autor: Johann Caro Burnett
% Lee archivos ASC-II de matriz de contiguidad, *.gal, creados en GEODA y
% los transforma en matrices de contiguidad W (de unos y ceros) y N
% (estandarizada)
 


% Lee tamaño de matriz de contiguidad = max
fid = fopen(filename);
c=fgets(fid);
max=getn(2,c);


% define matriz de ceros
W=zeros(max,max);

 
% repetir max veces el proceso
for i=1:max

% obtiene número de regiones contiguas, b
c=fgets(fid);
b=getn(2,c);

%salta a la fila que indica las coordenadas de regiones contiguas
c=fgets(fid);

for h=1:b
    
% getn: funcion que obtiene la posición h de una fila de datos
    j=getn(h,c);
    W(i,j)=1;
    
end

end


for i=1:max

S(i,i)=sum(W(i,:));

if S(i,i)~=0
    N(i,:)=W(i,:)/S(i,i);
end


end