#!/usr/bin/env python

__author__ = "bt3"


''' Given a sorted an array with empty strings,
    we use binary search to find some string (since the list is sorted):
    --> we deal with the empty strings with strip and then run to left
    and right, or move mid to the closed non-empty str (remember that
    the index must be conserved):
'''


def find_str_array_with_empty_str(seq, s1):
    if not seq or not s1:
        return None
    hi = len(seq)
    lo = 0

    while hi > lo:
        mid = (hi+lo)//2

        if seq[mid] == '':
            while True:
                left = mid-1
                right = mid+1
                if left < lo or right > hi: return None
                elif right < hi and seq[right]:
                    mid = right
                    break
                elif left > lo and seq[left]:
                    mid = left
                    break
                right += 1
                left -= 1

        if s1 == seq[mid] == s1:
            return mid
        elif s1 < seq[mid]:
            hi = mid
        else:
            lo = mid + 1



def test_find_str_array_with_empty_str():
    seq = ['acre', 'ball', '', 'coach', '', 'cut', '']
    key = seq[1]
    assert(find_str_array_with_empty_str(seq, key) == 1)
    print('Tests passed!')


if __name__ == '__main__':
    test_find_str_array_with_empty_str()
