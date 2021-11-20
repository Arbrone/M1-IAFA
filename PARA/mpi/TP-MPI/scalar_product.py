# /usr/bin/python3

import random

def scalar_product(X, Y):
    result = 0
    for i in range(len(X)):
        result += X[i]*Y[i]
    return result

if __name__ == '__main__':
    size = 10
    random.seed(0)
    
    X = [random.random() for _ in range(size)]
    Y = [random.random() for _ in range(size)]

    result = scalar_product(X, Y)
    print(result)
