"""Throwaway script to test computation of mutual information and BIC score
"""

from gPy.Data import Data
b = (0,1)
data = Data(variables='ABCD',domain='new',new_domain_variables={'A':b,'B':b,'C':b,'D':b})
records = (
    (0,0,1,1,2),
    (0,1,1,1,5),
    (1,0,1,0,7),
    (1,0,1,1,8),
    (1,1,1,1,4))
data.populate(records)
print data.mutual_information('A','B')
print data.mutual_information('A','C')
print data.mutual_information('B','B')
print data.mutual_information('B','A')
print data.mutual_information('B','AC')
print data.mutual_information('B','ACD')

print 'Entropies'
print data.entropy('A')
print data.entropy('C')
print data.entropy('AC')

print 'A'
print data.bic_search('A')
print 'B'
print data.bic_search('B')
print 'C'
print data.bic_search('C')
print 'D'
print data.bic_search('D')
