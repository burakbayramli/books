infile = open('read_pairs2.dat', 'r')
lines = infile.readlines()

pairs = []   # list of (n1, n2) pairs of numbers
for line in lines:
    line = line.strip()  # remove whitespace such as newline
    line = line.replace(' ', '')  # remove all blanks
    words = line.split(')(')
    # strip off leading/trailing parenthesis in first/last word:
    words[0] = words[0][1:]      # (-1,3  ->  -1,3
    words[-1] = words[-1][:-1]   # 8.5,9) ->  8.5,9
    for word in words:
        n1, n2 = word.split(',')
        n1 = float(n1);  n2 = float(n2)
        pair = (n1, n2)
        pairs.append(pair)

infile.close()
import pprint
pprint.pprint(pairs)

