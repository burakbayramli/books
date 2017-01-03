#!/usr/bin/env python

__author__ = "bt3"


''' computes the edit distance between two strings '''


def find_edit_distance(str1, str2):
    '''
    >>> s = 'sunday'
    >>> t = 'saturday'
    >>> find_edit_distance(s, t)
    3
    '''

	m = len(str1)
	n = len(str2)
	diff = lambda c1, c2: 0 if c1 == c2 else 1

	E = [[0] * (n + 1) for i in range(m + 1)]

	for i in range(m + 1):
		E[i][0] = i

    for j in range(1, n + 1):
		E[0][j] = j

    for i in range(1, m + 1):
		for j in range(1, n + 1):
			E[i][j] = min(E[i-1][j] + 1, E[i][j-1] + 1, E[i-1][j-1] + diff(str1[i-1], str2[j-1]))

    return E[m][n]



if __name__ == '__main__':
    import doctest
    doctest.testmod()




