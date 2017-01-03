#!/usr/bin/env python

__author__ = "bt3"


class HashTable(object):
    def __init__(self, slots=10):
        self.slots = slots
        self.table = []
        self.create_table()

    # Get the slot
    def hash_key(self, value):
        return hash(value)%self.slots

    # When creating the table, add list struct 
    # to each slot
    def create_table(self):
        for i in range(self.slots):
            self.table.append([])

    # Method to add a item in the right slot
    def add_item(self, value):
        key = self.hash_key(value)
        self.table[key].append(value)

    # Aux: print table
    def print_table(self):
        for key in range(self.slots):
            print "Key is {0}, value is {1}.".format(key, self.table[key])

    # Aux: find item
    def find_item(self, item):
        item_hash = self.hash_key(item)
        return item in self.table[item_hash]
        

if __name__ == '__main__':
    dic = HashTable(5)
    for i in range(1, 40, 2):
        dic.add_item(i)

    dic.print_table()
    assert(dic.find_item(20) == False)
    assert(dic.find_item(21) == True)