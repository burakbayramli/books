function Mnew=pivot(M,p,q)
%Mnew=pivot(M,p,q)
%Returns the matrix Mnew resulting from pivoting about the 
%(p,q)th element of the given matrix M.
%E.K.P. Chong, Mar. 23, 1994

for i=1:size(M,1),
  if i==p
    Mnew(p,:)=M(p,:)/M(p,q);
  else
    Mnew(i,:)=M(i,:)-M(p,:)*(M(i,q)/M(p,q));
  end %if
end %for
