def colour(position):
    x = position[0]
    y = position[1]
    if x%2 == 0:
        if (x+y)%2 == 0:
            return 'blue'
        else:
            return 'green'
    else:
        if (x*y)%2 == 0:
            return 'blue'
        else:
            return 'red'
