% script file for EFR 3.1:  listp2ver2
power =2; vector = [ ]; %start off with empty vector
while power <= n
    vector = [vector power];
    power=2*power;
end, vector
