from gPy.Examples import asia
from gPy.Samplers import BNSampler

sampler = BNSampler(asia)
cancer_index = sampler.variable_index('Cancer')
xray_index = sampler.variable_index('XRay')

for size in 10,100,1000,10000:
    print 'Sample size: ', size
    for j in range(5):
        sample = []
        for i in xrange(size):
            sample.append(sampler.forward_sample())
        cancer_count, both_count = 0, 0
        for inst in sample:
            if inst[cancer_index] == 'absent':
                cancer_count += 1
                if inst[xray_index] == 'abnormal':
                    both_count += 1
        print 'Estimate of P(Cancer=absent): ', cancer_count/float(size)
        print 'Estimate of P(Cancer=absent,XRay=abnormal): ', both_count/float(size)
        try:
            print 'Estimate of P(XRay=abnormal|Cancer=absent): ', both_count/float(cancer_count)
        except ZeroDivisionError:
            print '0/0!'
        print
