def npv_f(rate, cashflows):
    total = 0.0
    for i, cashflow in enumerate(cashflows):
<<<<<<< HEAD
        total += cashflow / (1 + rate)**1
=======
        total += cashflow / (1 +rate)**i
>>>>>>> python for finance
    return total
