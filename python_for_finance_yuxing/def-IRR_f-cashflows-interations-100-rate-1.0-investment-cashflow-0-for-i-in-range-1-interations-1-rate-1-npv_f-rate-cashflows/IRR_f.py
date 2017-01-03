def IRR_f(cashflows, interations=100):
    rate=1.0
    investment=cashflow[0]
    for i in range(1, interations+1):
        rate*=(1-npv_f(rate,cashflows)/investment)
    return rate
        
