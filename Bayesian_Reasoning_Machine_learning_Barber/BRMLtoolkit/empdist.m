function edist=empdist(data,v,X)
if length(X)>1
    px=reshape(normp(count(data(v,:),X)),X);
else
    px=normp(count(data(v,:),X));
end
edist.variables=v; edist.table=px;