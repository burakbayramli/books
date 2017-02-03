def read_file(filename):
    infile = open(filename, 'r')
    infile.readline()  # read column headings
    dates = [];  prices = []
    for line in infile:
        columns = line.split(',')
        date = columns[0]
        date = date[:-3]  # skip day of month
        price = columns[-1]
        dates.append(date)
        prices.append(float(price))
    infile.close()
    dates.reverse()
    prices.reverse()
    return dates, prices

dates = {};  prices = {}
d, p = read_file('stockprices_Sun.csv')
dates['Sun'] = d;  prices['Sun'] = p
d, p = read_file('stockprices_Microsoft.csv')
dates['MS'] = d;  prices['MS'] = p
d, p = read_file('stockprices_Google.csv')
dates['Google'] = d;  prices['Google'] = p

data = {'prices': prices, 'dates': dates}

# Normalize prices
norm_price = prices['Sun'][0]
prices['Sun'] = [p/norm_price for p in prices['Sun']]
norm_price = prices['MS'][0]
prices['MS'] = [p/norm_price for p in prices['MS']]

jan05_MS = prices['MS'][dates['MS'].index('2005-01')]
jan05_Sun = prices['Sun'][dates['Sun'].index('2005-01')]
norm_price = prices['Google'][0]/max(jan05_MS, jan05_Sun)
prices['Google'] = [p/norm_price for p in prices['Google']]

# Let the "x" values in the plot just be the indices
x = {}
x['Sun'] = range(len(prices['Sun']))
x['MS']  = range(len(prices['MS']))

# For google we must start on the corresponding Sun/MS index
# for January 2005
jan05 = dates['Sun'].index('2005-01')
x['Google'] = range(jan05, jan05 + len(prices['Google']), 1)

from scitools.std import plot
plot(x['MS'], prices['MS'], 'r-',
     x['Sun'], prices['Sun'], 'b-',
     x['Google'], prices['Google'], 'y-',
     legend=('Microsoft', 'Sun', 'Google'),
     savefig='tmp.eps')
