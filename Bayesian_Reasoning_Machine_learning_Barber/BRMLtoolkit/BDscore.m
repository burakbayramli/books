function score=BDscore(dataV,dataParents,nstatesV,nstatesPa,varargin)
%BDscore Bayes Dirichlet score of a node in Belief Network
% score=BDscore(dataV,dataParents,nstatesV,nstatesPa,<U>)
% U is the optional Dirichlet hyperparameter
% There is no explicit prior on the network structure
if isempty(varargin)
    U= 1./(nstatesV*prod(nstatesPa))*ones(nstatesV,prod(nstatesPa));
else
    U=varargin{1}*ones(nstatesV,prod(nstatesPa));
end
cvpa=count(vertcat(dataV,dataParents),[nstatesV nstatesPa]); % joint count
CvgPa = reshape(cvpa,nstatesV,prod(nstatesPa)); % conditional count
Up = U + CvgPa;
score= sum(gammaln(sum(U,1))-gammaln(sum(Up,1)) + sum(gammaln(Up),1)-sum(gammaln(U),1));