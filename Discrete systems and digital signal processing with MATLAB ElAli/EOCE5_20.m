num = [1 0 1 0];
den = [1 1 0 1];
poles_sys1 = roots([1 1 0 1])
impulse(num, den)
tittle('Impulse response of system 1')