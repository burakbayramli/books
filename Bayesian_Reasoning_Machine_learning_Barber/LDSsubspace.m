function [A,B,diagS,h,vest]=LDSsubspace(v,H,L)
%LDSSUBSPACE Subspace Method for identifying Linear Dynamical System
%[A,B,diagS,h,vest]=LDSsubspace(v,H,L)
% 
% Inputs:
% v : V x T data matrix
% H : dimension of Hidden space
% L : Stacking Order for the Hankel matrix
%
% Outputs:
% A : returned transition matrix
% B : returned emission matrix
% diagS : diagonal of the SVD of the Hankel matrix
% vest : reconstructions of the data
[V T]=size(v);
% Form the Hankel matrix:
for t=1:T-L+1
	col=[];
	for l=0:L-1
		col=vertcat(col,v(:,t+l));
	end
	Hank(:,t)=col;
end
[uu ss vv]=svd(Hank);diagS=diag(ss);
B = uu(1:V,1:H);
tmp =  ss*vv';
h = tmp(1:H,:);
vest = B*h; % reconstructions
A = h(:,2:end)*pinv(h(:,1:end-1)); % estimated transition