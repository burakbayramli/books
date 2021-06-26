function [pred_out] = pred(w1_M1_sample,w1_P1_sample,probe_vec,mean_rating);

%%% Make predicitions on the validation data

 aa_p   = double(probe_vec(:,1));
 aa_m   = double(probe_vec(:,2));
 rating = double(probe_vec(:,3));

 pred_out = sum(w1_M1_sample(aa_m,:).*w1_P1_sample(aa_p,:),2) + mean_rating;
 ff = find(pred_out>5); pred_out(ff)=5;
 ff = find(pred_out<1); pred_out(ff)=1;


 
