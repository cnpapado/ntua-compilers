def fib(n):
    i = 1
    f1 = 1
    f2 = 1

    while i < n-1:
        tmp = f1 + f2
        f1 = f2
        f2 = tmp
        i = i+1

    return f2

def fib_rec(n):
    if n <= 2: return 1
    else: return fib_rec(n-1) + fib_rec(n-2)

def fib_mem (n):
    def aux(n):
        if arr[n] == -1: 
            arr[n] = aux(n-1) + aux(n-2)
        return arr[n]

    i = 0
    arr = []
    while i <= n+2:
        arr.append(-1) 
        i += 1
    arr[1] = arr[2] = 1
  
    return aux(n)



for i in range(1,100):
    print(fib(i), fib_rec(i), fib_mem(i))