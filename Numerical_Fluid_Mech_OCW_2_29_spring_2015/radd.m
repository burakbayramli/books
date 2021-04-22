function c = radd(a,b,n)
%
% function c = radd(a,b,n)
%
% Adds two real numbers a and b simulating an arithmetic unit with
% n significant digits, and rounding-off (not chopping-off) of numbers.
% If the inputs a and b provided do not have n digits, they are first
% rounded to n digits before being added.

%--- First determine signs
sa=sign(a);
sb=sign(b);

%--- Determine the largest number (exponent) 
if (sa == 0)
    la=-200; %this makes sure that if sa==0, even if b is very small, it will have the largest exponent
else
la=ceil(log10(sa*a*(1+10^(-(n+1))))); %This determines the exponent on the base. Ceiling is used
                                     %since 0<log10(mantissa_base10)<=-1. The 10^etc. term just
                                     %properly increases the exponent estimated by 1 in the case 
                                     %of a perfect log: i.e. log10(m b^e) is an integer, 
                                     %mantissa is 0.1, hence log10(m)=-1, and
                                     %ceil(log10(m b^e(1+10^-(n+1))) ~< ceil(e +log10(m)+log10(1+10^-(n+1)))=e.  
end
if (sb == 0)
    lb=-200;
else
    lb=ceil(log10(sb*b*(1+10^(-(n+1)))));
end
    lm=max(la,lb);

%--- Shift the two numbers magnitude to obtain two integers with n digits
f=10^(n);   %this is used in conjunction with the round function below
at=sa*round(f*sa*a/10^lm);	%sa*a/10^lm shifts the decimal point such that the number starts with 0.something
                            %the f*(*) then raises the number to a power 10^n, to get the desired accuracy 
                            %of n digits above the decimal. After rounding to an integer, any figures that
                            %remain below are wiped out.
bt=sb*round(f*sb*b/10^lm);
% Check to see if another digit was added by the round. If yes, increase
% la (lb) and reset lm, at and bt. 
ireset=0;
if ((at~=0) & (log10(at)>=n))
    la=la+1; ireset=1;
end
if ((bt~=0) & (log10(bt)>=n))
    lb=lb+1; ireset=1;
end
if (ireset)
    lm=max(la,lb);
    at=sa*round(f*sa*a/10^lm);
    bt=sb*round(f*sb*b/10^lm);
end
ct=at+bt;   %adds the two numbers
sc=sign(ct);

%The following accounts for the case when another digit is added when
%summing two numbers... ie. if the number of digits desired is only 3, 
%then 999 +3 = 1002, but to keep only 3 digits, the 2 needs to be wiped out.
if (sc ~= 0)
    if (log10(sc*ct) >= n)  
        ct=round(ct/10)*10;
%        'ct'
    end
end

%-----This basically reverses the operation on line 34,38 
%  (it brings back the final number to its true magnitude) 
c=ct*10^lm/f;
