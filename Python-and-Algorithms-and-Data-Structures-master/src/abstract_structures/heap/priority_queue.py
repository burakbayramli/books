#!/usr/bin/env python

__author__ = "bt3"

import heapq

class PriorityQueue(object):
    ''' implements a priority queue class '''
    def __init__(self):
        self._queue = []
        self._index = 0 # comparying same priority level

    def push(self, item, priority):
        heapq.heappush(self._queue, (-priority, self._index, item))
        self._index += 1

    def pop(self):
        return heapq.heappop(self._queue)[-1]


class Item:
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return "Item({!r})".format(self.name)


def test_PriorityQueue():
    ''' push and pop are all O(logN) '''
    q = PriorityQueue()
    q.push(Item('test1'), 1)
    q.push(Item('test2'), 4)
    q.push(Item('test3'), 3)
    assert(str(q.pop()) == "Item('test2')")


if __name__ == '__main__':
    test_PriorityQueue()


