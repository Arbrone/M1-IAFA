def syracuse(n):
    steps = 0
    while n != 1:
        if n%2 == 0:
            n = n//2
        else:
            n = 3*n+1
        steps += 1
    return steps

def total_steps(n):
    if(n > 1):
        return syracuse(n) + total_steps(n-1)
    else:
        return 0

print(total_steps(4))