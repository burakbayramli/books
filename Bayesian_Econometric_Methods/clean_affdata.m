%clean the fair data from the PT survey;
clear;
clc;
load affdata2.txt;

const = affdata2(:,3);
male = affdata2(:,4);
ys_married = affdata2(:,6);
kids = affdata2(:,7);
relig = affdata2(:,8);
ed = affdata2(:,9);
happ_mar = affdata2(:,12);
num_aff = affdata2(:,13);

%clean up the data
nobs = length(ed);
affair = zeros(nobs,1);
religious = zeros(nobs,1);
happy = affair;
for i = 1:nobs;
    if num_aff(i,1) > 0
        affair(i,1) =1;
    end;
    if relig(i,1) >3;
        religious(i,1)=1;
    end;
    if happ_mar(i,1) >=4;
        happy(i,1) =1;
    end;
end;

%Run the probit model using this data


x = [const male ys_married kids religious ed happy];
xbar = [1 mean(male) mean(ys_married) mean(kids) mean(religious) mean(ed) mean(happy)]; %the male, kids, religious and happy variables could be set to 
                                                                                %integer values instead
xbar1a = [1 1  mean(ys_married) mean(kids) mean(religious) mean(ed) mean(happy)];
xbar1b = [1 0  mean(ys_married) mean(kids) mean(religious) mean(ed) mean(happy)];

xbar2a = [1 mean(male) mean(ys_married) 1 mean(religious) mean(ed) mean(happy)];
xbar2b = [1 mean(male) mean(ys_married) 0 mean(religious) mean(ed) mean(happy)];

xbar3a = [1 mean(male) mean(ys_married) mean(kids) 1 mean(ed) mean(happy)];
xbar3b = [1 mean(male) mean(ys_married) mean(kids) 0 mean(ed) mean(happy)];

xbar4a = [1 mean(male) mean(ys_married) mean(kids) mean(religious) mean(ed) 1];
xbar4b = [1 mean(male) mean(ys_married) mean(kids) mean(religious) mean(ed) 0];

y = affair;
%Specify prior values
mubeta = zeros(size(x,2),1);
varbeta = 10^2*eye(size(x,2));
invarbeta = inv(varbeta);


iter = 2000;
burn = 500;

me_male_final = zeros(iter-burn,1);
me_ys_married_final = me_male_final;
me_kids_final = me_male_final;
me_religious_final = me_male_final;
me_ed_final = me_male_final;
me_happy_final = me_male_final;
beta_final = zeros(iter-burn,size(x,2));
beta = zeros(size(x,2),1);

%begin the Gibbs Sampler
for i = 2:iter;i
   %For the latent data
   zaug = truncnorm(x*beta,ones(nobs,1),y);
   
   %do beta part
   D_beta = inv(x'*x + invarbeta);
   d_beta = x'*zaug + invarbeta*mubeta;
   H = chol(D_beta);
   beta = D_beta*d_beta + H'*randn(size(x,2),1);
   
   %calculate marginal effects;
   me_male = normcdf(xbar1a*beta) - normcdf(xbar1b*beta);
   me_ysmarried = beta(3)*normpdf(xbar*beta);
   me_kids = normcdf(xbar2a*beta) - normcdf(xbar2b*beta);
   me_religious = normcdf(xbar3a*beta) - normcdf(xbar3b*beta);
   me_ed = beta(6)*normpdf(xbar*beta);
   me_happy = normcdf(xbar4a*beta) - normcdf(xbar4b*beta);
  
   
   if i > burn;
       beta_final(i-burn,:) = beta';
       me_male_final(i-burn,1) = me_male;
       me_ysmarried_final(i-burn,1) = me_ysmarried;
       me_kids_final(i-burn,1) = me_kids;
       me_religious_final(i-burn,1) = me_religious;
       me_ed_final(i-burn,1) = me_ed;
       me_happy_final(i-burn,1) = me_happy;
   end;
end;
    