def fib(n, current=3, prev1=1, prev2=1):
  if n == 0 or n == 1:
    return 1
  elif current >= n:
    return prev1 + prev2
  else:
    return fib(n, current + 1, prev1 + prev2, prev1)

answer = fib(5)
print answer

answer = fib(7)
print answer

