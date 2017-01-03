#!/usr/bin/env python

__author__ = "bt3"

''' a class for a double ended queue (also inefficient) '''

from queue import Queue

class Deque(Queue):

    def enqueue_back(self, item):
        self.items.append(item)

    def dequeue_front(self):
        return self.items.pop(0)



if __name__ == '__main__':
    queue = Deque()
    print("Is the queue empty? ", queue.isEmpty())
    print("Adding 0 to 10 in the queue...")
    for i in range(10):
        queue.enqueue(i)
    print("Queue size: ", queue.size())
    print("Queue peek : ", queue.peek())
    print("Dequeue...", queue.dequeue())
    print("Queue peek: ", queue.peek())
    print("Is the queue empty? ", queue.isEmpty())
    print(queue)

    print("\nNow using the dequeue methods...")
    print("Dequeue from front...", queue.dequeue_front())
    print("Queue peek: ", queue.peek())
    print(queue)
    print("Queue from back...")
    queue.enqueue_back(50)
    print("Queue peek: ", queue.peek())
    print(queue)
