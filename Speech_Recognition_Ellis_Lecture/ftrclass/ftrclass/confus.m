function M = confus(R,A)
% M = confus(R,A)  Calculate a confusion matrix
%       R is a set of reference label indices, and A is the actual labels 
%       assigned by the classifier.  M returns an NxN matrix, where 
%       each row counts the number of times the data that was truly in 
%       the corresponding class is classified to belong to the 
%       class corresponding to the column.  Off-diagonal elements
%       indicate classifier errors.
% 2001-03-28 dpwe@ee.columbia.edu

nclass = max([R,A]);

M = zeros(nclass,nclass);

for srcclass = 1:nclass

  % Which rows really belong to this class
  thisclsix = find(R == srcclass);

  % Count each type of output

  for dstclass = 1:nclass

    M(srcclass, dstclass) = sum(A(thisclsix) == dstclass);

  end

end
