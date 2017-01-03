function mdraw = multi_rnd(probs)
% PURPOSE: a vector of random draws from the Multinomial distribution
%with T = 1 

kdim=size(probs,1);
mdraw=zeros(kdim,1);
psum=0;
udraw=rand;
for i = 1:kdim
  psum=psum+probs(i,1);
    if udraw<psum
        mdraw(i,1)=1;
        break
    end
end


    

