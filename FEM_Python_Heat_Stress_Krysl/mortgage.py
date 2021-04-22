def findPayment(loan, r, m):


    """Assumes: loan and r are floats, m an int
    Returns the monthly payment for a mortgage of size
    loan at a monthly rate of r for m months"""
    return loan * ((r * (1 + r) ** m) / ((1 + r) ** m - 1))

print(findPayment(150000, 0.0325/12, 60))