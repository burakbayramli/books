#!/usr/bin/env python

__author__ = "bt3"

def simple2(a, *args):
    print args

def simple(*args):
    print args

def simple3(**kwargs):
    print kwargs


simple(1, 2, 3)
simple2(1, 2, 3)
simple3(x=1, y=2)
