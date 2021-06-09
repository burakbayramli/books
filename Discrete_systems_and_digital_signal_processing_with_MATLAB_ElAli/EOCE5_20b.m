num = [1 1 1];
den = [1 4 3 0];
poles_sys1 = roots([1 4 3 0])
impulse(num, den)
tittle('Impulse system of system 2')