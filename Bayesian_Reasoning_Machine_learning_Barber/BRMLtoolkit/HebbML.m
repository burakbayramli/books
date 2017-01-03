function [W,theta,loglik]=HebbML(v,eta,epochs,varargin)
%HEBBML Learn a sequence for a Hopfield Network
% [W,theta,loglik]=HebbML(v,eta,epochs,<opts>)
% Learn the sequence of vectors v(:,1:T) using simple gradient ascent of
% Likelihood under a simple Hopfield network
%
% Inputs:
% v : sequence matrix : v(:,t) is the vector of binary states (1,0) at time t
% eta : learning rate
% epochs : number of gradient updates
% opts.plotpress=1 : draw log likelihood
%
% Outputs:
% W : the Hopfield network interaction matrix
% theta : the biases
% loglik : log likelihood of the sequence log p(v(1:T))
% See also demoHopfield.m
if isempty(varargin); opts.plotprogress=1; else opts=varargin{1};end
N=length(v); % number of sequences
[V T] = size(v{1});
W = 0.001*randn(V,V); % random initialisation
theta = zeros(V,1);

for learn = 1:epochs % learning epochs
    grad = zeros(V,V); ll =0;
    grad_th = zeros(V,1);
    for n=1:N
        vn = v{n};
        for t =1 :T-1
            sig_it = sigma((vn(:,t+1)).*(theta + W*vn(:,t)));
            grad = grad + (1-sig_it).*(vn(:,t+1))*vn(:,t)'; 
            grad_th = grad_th + (1-sig_it).*(vn(:,t+1)); 
            ll = ll + sum(log(sig_it));
        end
    end
    W = W +eta*grad;  % simple gradient ascent
    theta = theta +eta*grad_th;
    loglik(learn)=ll;
    if opts.plotprogress; plot(loglik); drawnow; end
end