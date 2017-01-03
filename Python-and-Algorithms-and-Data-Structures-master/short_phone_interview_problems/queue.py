#!/usr/bin/env python

__author__ = "bt3"


class Queue(object):
    def __init__(self):
        self.enq = []
        self.deq = []

    def enqueue(self, item):
        return self.enq.append(item)

    def deque(self):
        if not self.deq:
            while self.enq:
                self.deq.append(self.enq.pop())
        return self.deq.pop()

    def peak(self):
        if not self.deq:
            while self.enq:
                self.deq.append(self.enq.pop())
        if self.deq:
            return self.deq[-1]

    def size(self):
        return len(self.enq) + len(self.deq)

    def isempty(self):
        return not (self.enq + self.deq)

    
if __name__ == '__main__':
    q = Queue()
    for i in range(1,11):
        q.enqueue(i)
    print 'Size:',  q.size()
    print 'Is empty?', q.isempty()
    print 'Peak: ', q.peak()
    print
    print 'Dequeuing...'
    for i in range(10):
        print q.deque()
    print 'Size:',  q.size()
    print 'Is empty?', q.isempty()
    print 'Peak: ', q.peak()