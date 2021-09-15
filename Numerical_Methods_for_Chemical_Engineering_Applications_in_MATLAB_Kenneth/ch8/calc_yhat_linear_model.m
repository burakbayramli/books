% calc_yhat_linear_model.m
% This routine generates the vector of
% predictions for a standard linear
% model.
% K.J. Beers. MIT ChE. 12/8/2004

function y_hat = calc_yhat_linear_model(theta,X);

y_hat = X*theta;

return;
