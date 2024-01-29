% Compute strain-displacement matrix
function B= CompB4x8Quad4atPoint(Nxi_vec, Neta_vec)
B=zeros(4,8);  
for i=1:4
  B(1,2*i-1)=Nxi_vec(i);
  B(3,2*i)= Nxi_vec(i);
  B(2,2*i-1)=Neta_vec(i);
  B(4,2*i)= Neta_vec(i);
end