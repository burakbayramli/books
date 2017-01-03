'''Example of a Kruskal-Wallis test (for not normally distributed data)
Taken from http://www.brightstat.com/index.php?option=com_content&task=view&id=41&Itemid=1&limit=1&limitstart=2

'''

'''
Author: Thomas Haslwanter
Date:   May-2013
Ver:    1.2
'''

from scipy.stats.mstats import kruskalwallis
from numpy import array

def main():
    # Get the data
    city1 = array([68, 93, 123, 83, 108, 122])
    city2 = array([119, 116, 101, 103, 113, 84])
    city3 = array([70, 68, 54, 73, 81, 68])
    city4 = array([61, 54, 59, 67, 59, 70])
    
    # Perform the Kruskal-Wallis test
    h, p = kruskalwallis(city1, city2, city3, city4)
    
    # Print the results
    if p<0.05:
        print('There is a significant difference between the cities.')
    else:
        print('No significant difference between the cities.')
        
    return h

if __name__ == '__main__':
    main()    