clear all

N=4
H=bsxfun(@plus,[1:N]',0:N-1)
H=1./H
iH= inv(H)
cond(H)