def npv_f(rate, cashflows):
    total = 0.0
    for i, cashflow in enumerate(cashflows):
        total += cashflow / (1 +rate)**i
    return total
def IRR_f(cashflow, iterations=100):
    if len(cashflows)==0:
        print 'number of cash flows is zero'
        return -99
    rate = 1.0
    investment = cashflows[0]
    for i in range(1, iterations+1):
        rate *= (1 - npv_f(rate, cashflows) / investment)
    return rate
