############## Poolability: Chow test
# Baltagi (2013), Econometric Analysis of Panel Data, 5th edition, Wiley & Sons
# Sec 4.1.3, example 2, p. 68 => results are replicated

data("Gasoline", package = "plm")
form <- lgaspcar ~ lincomep + lrpmg + lcarpcap

# poolability across countries
pooltest(form, data = Gasoline, effect = "individual", model = "pooling") # matches: F=129.38 [F(68,270)]

# poolability across countries [slope coefficients only, allowing for different intercepts]
pooltest(form, data = Gasoline, effect = "individual", model = "within")  # matches: F= 27.33 [F(51,270)]

# poolability across time
pooltest(form, data = Gasoline, effect = "time", model = "pooling") # matches: F= 0.276 [F(72,266)]
pooltest(form, data = Gasoline, effect = "time", model = "within")  # no value stated in Baltagi (2013) for within
