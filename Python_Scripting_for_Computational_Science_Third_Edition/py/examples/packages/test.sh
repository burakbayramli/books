#!/bin/sh -x
cd orig
python test1.py
cd ../new
python test1.py
cd ../new2
python test1.py

