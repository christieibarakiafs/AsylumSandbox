def calc_mean(x):
    sum = 0
    for int in x:
        sum += int
    return sum / len(x)

def check_prime (x):
    if x <= 1:
        return False
    if x > 1:
        for i in range(2,x):
            if x % i == 0:
                return False
        return True

def GCF_calc(a, b):
    if b == 0:
        return a
    else:
        return GCF_calc(b, a%b)

def sqr(x):
    j = x**0.5
    return j

