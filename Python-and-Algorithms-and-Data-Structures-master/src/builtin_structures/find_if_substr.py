#!/usr/bin/env python

__author__ = "bt3"


def find_substr(s1, s2):

    if len(s1) < len(s2):
        bs = s2
        ss = s1
    else:
        bs = s1
        ss = s2

    ps = 0

    for c in bs:

        if ss[ps] == c:
            ps += 1
        else:
            ps = 0

        if ps == len(ss)-1:
            return True

    return False



if __name__ == '__main__':
    s1 = 'buffy is a vampire slayer'
    s2 = 'vampire'
    s3 = 'angel'
    assert(find_substr(s2, s1) == True)
    assert(find_substr(s3, s1) == False)