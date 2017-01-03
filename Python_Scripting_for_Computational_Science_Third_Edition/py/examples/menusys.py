#!/usr/bin/env python

"""
Simple menu system
"""
import os, sys

class Submenu:
    def __init__(self, name, description):
        self.name = name
        self.description = description
        self.items = []

    def __str__(self):
        return self.brief_dump()
    
    INDENT='  '   # indentation used in printout
    def brief_dump(self, indent=''):
        """print this submenu and all its submenus recursively"""
        s = ''
        for item in self.items:
            if isinstance(item, Submenu):
                s += indent + 'submenu: %s\n' % item.name
                s += item.brief_dump(indent+Submenu.INDENT)
            elif isinstance(item, dict):
                s += indent + 'item: ' + item['name'] \
                     + ' (%s)' % item['default'] + '\n'
        return s


# make a sample menu
menu = Submenu('main', 'toplevel of the menu system')
item = {'name' : 'no of grid points',
        'help' : None,
        'cmlarg' : '--npoints',
        'default' : '40'}
menu.items.append(item)
item = {'name' : 'stop time',
        'help' : 'simulate from time 0 to stop time',
        'cmlarg' : '--tstop',
        'default' : '10.0'}
menu.items.append(item)

submenu = Submenu('bottom functions', 'type of bottom functions')
menu.items.append(submenu)

submenu2 = Submenu('FlatBottom', 'bottom with constant depth')
submenu.items.append(submenu2)
item = {'name' : 'FlatBottom depth',
        'help' : 'constant depth',
        'cmlarg' : '--FlatBottom-depth',
        'default' : '1.0'}
submenu2.items.append(item)

submenu2 = Submenu('BellBottom', 'Gaussian bell shaped bottom')
submenu.items.append(submenu2)
item = {'name' : 'BellBottom mean',
        'help' : 'location of the peak of the bottom function',
        'cmlarg' : '--BellBottom-mean',
        'default' : '5.0'}
submenu2.items.append(item)
item = {'name' : 'simulation description',
        'help' : 'what this simulation is about',
        'cmlarg' : None,
        'default' : 'some simulation'}
menu.items.append(item)

print menu
