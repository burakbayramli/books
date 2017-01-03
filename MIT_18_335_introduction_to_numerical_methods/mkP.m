function P=mkP(m,rows)
%MKP  Create m-by-m permutation matrix that interchanges rows.

P=eye(m);
P(rows,:)=P(rows(end:-1:1),:);
