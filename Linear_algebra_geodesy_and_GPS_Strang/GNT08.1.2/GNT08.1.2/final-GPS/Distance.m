function d=Distance(Pos_SV,Pos_Rcv);
[m,n]=size(Pos_SV);
for i=1:m
    d(i)=norm(Pos_SV(i,:)-Pos_Rcv);
end