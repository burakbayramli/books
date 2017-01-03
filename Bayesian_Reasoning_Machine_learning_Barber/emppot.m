function epot=empdist(data,v,X)
if length(X)>1
    px=reshape(normp(count(data(v,:),X)),X);
else
    px=normp(count(data(v,:),X));
end
epot.variables=v; epot.table=px;