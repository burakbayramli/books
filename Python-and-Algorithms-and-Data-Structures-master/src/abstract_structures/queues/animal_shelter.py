#!/usr/bin/env python

__author__ = "bt3"


""" A class for an animal shelter with two queues"""


class Node(object):
    def __init__(self, animalName=None, animalKind=None, pointer=None):
        self.animalName = animalName
        self.animalKind = animalKind
        self.pointer = pointer
        self.timestamp = 0



class AnimalShelter(object):
    def __init__(self):
        self.headCat = None
        self.headDog = None
        self.tailCat = None
        self.tailDog = None
        self.animalNumber = 0


    # Queue any animal

    def enqueue(self, animalName, animalKind):
        self.animalNumber += 1
        newAnimal = Node(animalName, animalKind)
        newAnimal.timestamp = self.animalNumber

        if animalKind == 'cat':
            if not self.headCat:
                self.headCat = newAnimal
            if self.tailCat:
                self.tailCat.pointer = newAnimal
            self.tailCat = newAnimal

        elif animalKind == 'dog':
            if not self.headDog:
                self.headDog = newAnimal
            if self.tailDog:
                self.tailDog.pointer = newAnimal
            self.tailDog = newAnimal


    # Dequeue methods

    def dequeueDog(self):
        if self.headDog:
            newAnimal = self.headDog
            self.headDog = newAnimal.pointer
            return str(newAnimal.animalName)
        else:
            return 'No Dogs!'


    def dequeueCat(self):
        if self.headCat:
            newAnimal = self.headCat
            self.headCat = newAnimal.pointer
            return str(newAnimal.animalName)
        else:
            return 'No Cats!'


    def dequeueAny(self):
        if self.headCat and not self.headDog:
            return self.dequeueCat()
        elif self.headDog and not self.headCat:
            return self.dequeueDog()
        elif self.headDog and self.headCat:
            if self.headDog.timestamp < self.headCat.timestamp:
                return self.dequeueDog()
            else:
                return self.dequeueCat()
        else:
            return ('No Animals!')



    def _print(self):
        print("Cats:")
        cats = self.headCat
        while cats:
            print(cats.animalName, cats.animalKind)
            cats = cats.pointer
        print("Dogs:")
        dogs = self.headDog
        while dogs:
            print(dogs.animalName, dogs.animalKind)
            dogs = dogs.pointer


if __name__ == '__main__':

    qs = AnimalShelter()
    qs.enqueue('bob', 'cat')
    qs.enqueue('mia', 'cat')
    qs.enqueue('yoda', 'dog')
    qs.enqueue('wolf', 'dog')
    qs._print()

    print("Deque one dog and one cat...")
    qs.dequeueDog()
    qs.dequeueCat()
    qs._print()
