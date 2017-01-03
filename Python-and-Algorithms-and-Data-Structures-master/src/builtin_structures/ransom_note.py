#!/usr/bin/env python

__author__ = "bt3"



from collections import Counter

def check_if_ransom_note(magazines, note):
    count = Counter()
    pm, pn = 0, 0

    while pn < len(note) and pm < len(magazines):
        char_note = note[pn]
        if count[char_note]>0:
            count[char_note] -= 1
            pn += 1
        else:
            char_magazine = magazines[pm]
            count[char_magazine] += 1
            pm +=1

    return pn == len(note)



if __name__ == '__main__':

    magazines1 = "avfegthhgrebvkdsvnijnvyijfdmckdsmovkmmfvskumvl;cdkmioswckofjbkreenyukjemjgnmkmvkmnvdkmvkr g gmvdvmldm vldfkmbldkmlvdkm"
    magazines2 = "adfsfa"
    note = "you should disobey"

    print(check_if_ransom_note(magazines1, note))
    print(check_if_ransom_note(magazines2, note))
