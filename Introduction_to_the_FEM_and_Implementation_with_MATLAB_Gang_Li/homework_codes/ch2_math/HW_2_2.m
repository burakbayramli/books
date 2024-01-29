clear all;
A=rand(12,8)           

for i=1:12
  for j=1:8
    if A(i,j)<0.3 
      A(i,j)=0;
    else 
      A(i,j)=1;
    end
  end
end
A