function [ ] = sum2sq(n)
%M-file for EFR 4.2
for a=1:sqrt(n) 
    b=sqrt(n-a^2); %solve n=a^2+b^2 for b
    if b==floor(b); %checks to see if b is integer
        fprintf('the integer %d can be written as the sum of squares of %d and %d', n,a,b)
        return
    end
end
fprintf('the integer %d cannot be written as the sum of squares', n)

    