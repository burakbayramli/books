a = 0; b = 2; n = 1000;
t0 = cputime;
for i = 1:10000  % repetitions to obtain some seconds CPU time
    result = Trapezoidal(a, b, @f1, n);
end
disp(result);
t1 = cputime - t0;
disp(t1);
exit
