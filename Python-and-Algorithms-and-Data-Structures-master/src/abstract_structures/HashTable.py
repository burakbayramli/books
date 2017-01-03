#!/usr/bin/env python

__author__ = "bt3"


class HashTable(object):
    def __init__(self, slots=10):
        self.slots = slots
        self.table = []
        self.create_table()

    def hash_key(self, value):
        return hash(value)%self.slots

    def create_table(self):
        for i in range(self.slots):
            self.table.append([])

    def add_item(self, value):
        key = self.hash_key(value)
        self.table[key].append(value)

    def print_table(self):
        for key in range(len(self.table)):
            print "Key is %s, value is %s." %(key, self.table[key])

    def find_item(self, item):
        pos = self.hash_key(item)
        if item in self.table[pos]:
            return True
        else:
            return False

if __name__ == '__main__':
    dic = HashTable(5)
    for i in range(1, 40, 2):
        dic.add_item(i)

    dic.print_table()
    assert(dic.find_item(20) == False)
    assert(dic.find_item(21) == True)
