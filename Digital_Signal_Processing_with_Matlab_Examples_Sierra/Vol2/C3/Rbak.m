
%Backprojections---------------------- 
aux=zeros(128,128); IR=aux;
for nn=1:MA:N,
   for np=1:128,
      aux(:,np)=PR(:,nn);
   end;
   IR=IR+imrotate(aux,theta(nn),'bilinear','crop');
end;
nIR=IR/max(max(IR)); %normalize
