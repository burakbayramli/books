function z = squareheatsourcevec(x,y)
for i=1:length(x)
    for j=1:length(y)        
if x(i)>=.25 & x(i)<=.5 & y(j)>=.65 & y(j)<=.9
    z(i,j)=-800;
else
    z(i,j)=0;
end
end
end
