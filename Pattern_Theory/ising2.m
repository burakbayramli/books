% % Remarks:
% You will need to call the following indexing code once after you
% have loaded J (nancy.mat). The Gibbs/Metropolis part of the code should
% be used as the inner loop in your cooling schedule.


%% INDEXING CODE %%
%% Create neighborhood indices, ind, on the grid of pixels of J.  
%% Note that in the Gibbs code, we convert both images I and J into
%% one long column so as to index its entries by a single integer:
%% I(1),...,I(Nt) runs through all entries of I. In this form, it
%% is not clear which are the 4 nearest neighbors of a pixel.
%% To get these indices, we use ind(:,k) computed below.
%% Figure out how the code below works, e.g. by running it on a
%% small test matrix and examining everything it does.
%% To make the indexing work, you MUST add a line like the following
%%
%% I = [I zeros(size(I,1),1)]; % takes I of size J and pads w/col of zeros.
%%
%% to make I into a Nr x (Nc+1) image of the form: [I Z] where Z is 
%% a Nr x 1 column of zeros. Why? Note that "even" and "odd" are also
%% needed in the Gibbs Sampler code below.
 
Nr = size(J,1); Nc = size(J,2); Nt = Nr*Nc;
[x y] = meshgrid(1:Nc, 1:Nr);
even = find(rem(x+y,2) == 0); Ne = size(even,1);
odd = find(rem(x+y,2) == 1); No = size(odd,1);
indx = ones(4,1)*x(:)' + [-1 1 0 0]'*ones(1,Nt);
indy = ones(4,1)*y(:)' + [0 0 -1 1]'*ones(1,Nt);
boundary = [find(indx>Nc) ; find(indx<1) ; find(indy>Nr) ; find(indy<1)]; 
ind = [(indx-1)*Nr + indy];
ind(boundary) = (Nt+1)*ones(size(boundary));
 
 
%% GIBBS SAMPLER CODE %%
%% Note that it proposes flipping each pixel once for each j.
%% THIS CODE IS FOR IMAGES WITH VALUES +1/-1


for j = 1:Nsweeps
        adj = sum(I(ind(:,odd)))';
        dE = I(odd).*(adj+c*J(odd));
%        acc = odd(exp(-dE/T)>rand(No,1));  % Metropolis
	acc = odd( [1./(1+exp(dE/T))] > rand(No,1) );  % Gibbs
        I(acc) = -I(acc);
 
        adj = sum(I(ind(:,even)))';
        dE = I(even).*(adj+c*J(even));
%        acc = even(exp(-dE/T)>rand(Ne,1)); % Metropolis
	acc = even( [1./(1+exp(dE/T))] > rand(Ne,1) );  % Gibbs
        I(acc) = -I(acc);
end
 
 

