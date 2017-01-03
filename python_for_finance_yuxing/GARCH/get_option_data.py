def get_option_data(tickrr,exp_date):
x = Options(ticker,'yahoo')
puts,calls = x.get_options_data(expiry=exp_date)
return puts, calls
