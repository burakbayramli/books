import urllib
url = 'http://www.metoffice.gov.uk/climate/uk/stationdata/oxforddata.txt'
local_file = 'Oxford.txt'
urllib.urlretrieve(url, filename=local_file)

infile = open(local_file, 'r')
data = {}
data['place'] = infile.readline().strip()
data['location'] = infile.readline().strip()
# Skip the next 5 lines
for i in range(5):
    infile.readline()

data['data'] ={}
for line in infile:
    columns = line.split()

    year = int(columns[0])
    month = int(columns[1])

    if columns[-1] == 'Provisional':
        del columns[-1]
    for i in range(2, len(columns)):
        if columns[i] == '---':
            columns[i] = None
        elif columns[i][-1] == '*' or columns[i][-1] == '#':
            # Strip off trailing character
            columns[i] = float(columns[i][:-1])
        else:
            columns[i] = float(columns[i])

    tmax, tmin, air_frost, rain, sun = columns[2:]

    if not year in data['data']:
        data['data'][year] = {}
    data['data'][year][month] = {'tmax': tmax,
                                 'tmin': tmin,
                                 'air frost': air_frost,
                                 'sun': sun}
infile.close()
# pick a an item to verify that temps is correct:
# 2006   2    6.6     0.8      12    33.0    73.1
year = 2006; month = 2
print data['data'][year][month]
#import pprint; pprint.pprint(data)

sun = [[data['data'][y][m]['sun'] for m in range(1,13)] \
       for y in range(1929, 2010)]
pprint.pprint(sun)

    


