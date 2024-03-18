function D = baseDigits(N)
if N<10
    D = N;
else
    q = floor(N/10);
    D = [basedigits(q),N-q*10];
end