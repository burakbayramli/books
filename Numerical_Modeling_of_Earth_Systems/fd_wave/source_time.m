function f = source_time(t,half_dur)

%%%% Source time function

%%%% Gaussian function
f = exp(-200*(t-half_dur).^2);