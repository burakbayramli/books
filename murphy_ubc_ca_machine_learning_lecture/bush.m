N = 2000;
p1 = betarnd(295, 308, N, 1);
p2 = betarnd(289, 333, N, 1);
dif = (p2-p1);
mean(dif > 0)
