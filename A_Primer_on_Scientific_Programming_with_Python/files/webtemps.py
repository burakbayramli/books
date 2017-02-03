import urllib
url = 'ftp://ftp.engr.udayton.edu/jkissock/gsod/NOOSLO.txt'
urllib.urlretrieve(url, filename='Oslo.txt')

infile = open('Oslo.txt', 'r')
temps = {}
for line in infile:
    month, date, year, temperature = line.split()
    month = int(month)
    date = int(date)
    year = int(year)
    temperature = float(temperature)
    if not year in temps:
        temps[year] = {}
    if not month in temps[year]:
        temps[year][month] = {}
    temps[year][month][date] = temperature
infile.close()

# Pick a day to verify that temps is correct
year = 2003; month = 3; date = 31
T = temps[year][month][date]
print '%d.%d.%d: %.1f' % (year, month, date, T)


