def sum_xy(x, y):
    return x + y

def prod_xy(x, y):
    return x*y

def treat_xy(f, x, y):
    return f(x, y)

x = 2;  y = 3
print treat_xy(sum_xy, x, y)
print treat_xy(prod_xy, x, y)
