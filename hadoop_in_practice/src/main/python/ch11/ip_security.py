import sys, random

for line in sys.stdin:
  fields = line.rstrip("\n\r")
  sys.stdout.write("%s,%s\n" % (fields, random.randint(1, 10)))

