function lpost = ch6aw(pdiag,bdraw,hdraw,ystar,xstar,n)
%numerator or denominator of for acceptance probability in Ch 6

ldetomega=sum(log(pdiag));

%Likelihood term = posterior term for noninformative prior
lpost = -.5*ldetomega-.5*hdraw*(ystar-xstar*bdraw)'*(ystar-xstar*bdraw);




