def load(studentfile):
    infile = open(studentfile, 'r')
    data = {}
    while True:
        line = infile.readline()
        if not line: break
        i = line.find('Name:')
        if i != -1:
            # line contains 'Name:', extract the name
            name = line[i+5:]
            name = name.strip()  # strip off blanks
            data[name] = []
        elif line.isspace():     # blank line?
            continue             # go to next loop iteration
        else:
            # this must be a course line
            words = line.split()
            grade = words[-1]
            credit = int(words[-2])
            semester = ' '.join(words[-4:-2])
            course_name = ' '.join(words[:-4])
            data[name].append({'title': course_name,
                               'semester': semester,
                               'credit': credit,
                               'grade': grade})
    infile.close()
    return data

grade2numbers = {'A': 5, 'B': 4, 'C': 3, 'D': 2, 'E': 1, 'F': 0}
number2grades = {}  # "inverse" of grade2numbers
for grade in grade2numbers:
    number2grades[grade2numbers[grade]] = grade

def average_grade(data, name):
    sum = 0; weights = 0
    for course in data[name]:
        weight = course['credit']
        sum += grade2numbers[course['grade']]*weight
        weights += weight
    avg = sum/float(weights)
    return number2grades[round(avg)]

data = load('students.dat')
import pprint
pprint.pprint(data)
print 'Average grades:'
for name in data:
    print '%s: %s' % (name, average_grade(data, name))




