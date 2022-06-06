"""Chap 1-2. Iterative algorithm"""

import random


def put_id():
    x = 1
    print(f"id(x) = {id(x)}")


if __name__ == "__main__":
    # get sum of integers from 1 to n
    print("getting sum from 1 to n")
    n = int(input("enter n: "))

    # using while
    sum_while = 0
    i = 1
    while i <= n:
        sum_while += i
        i += 1
    print(f"The sum of integers from 1 to n is: {sum_while}")

    # using for
    sum_for = 0
    for i in range(n + 1):
        sum_for += i
    print(f"The sum of integers from 1 to n is: {sum_for}")

    # using gauss method
    sum_gauss = n * (n + 1) // 2
    print(f"The sum of integers from 1 to n is: {sum_gauss}")

    # get sum of integers from a to b
    print("Getting sum from a to b")
    a = int(input("Enter a: "))
    b = int(input("Enter b: "))

    if a > b:
        a, b = b, a

    sum = 0
    # bad way to use if
    for i in range(a, b + 1):
        if i < b:
            print(f"{i} + ", end="")
        else:
            print(f"{i} = ", end="")
        sum += i
    print(sum)

    # better way
    sum = 0
    for i in range(a, b):
        print(f"{i} + ", end="")
        sum += i
    print(f"{b} = ", end="")
    sum += b
    print(sum)
    print(f"The sum from a to b is {sum}")

    # exchanging variables
    # simple way
    a, b = b, a
    print(f"a = {a}, b = {b}")

    a, b = b, a
    # temporary variable
    t = a
    a = b
    b = t
    print(f"a = {a}, b = {b}")

    # printing + and - sign
    print("printing + and - rotatively")
    iter = int(input("Enter iteration time: "))

    # this program goes over if condition every time in the loop
    for i in range(iter):
        if i % 2:
            print("-", end="")
        else:
            print("+", end="")

    print()

    # better way
    for _ in range(iter // 2):
        print("+-", end="")  # print "+-"

    if iter % 2:
        print("+", end="")

    print()

    # alternative
    for _ in range(1, iter // 2 + 1):
        print("+-", end="")
    # iter can be odd number
    if iter % 2:
        print("+", end="")
    print()

    # print * n times, by breaking lines every w *'s
    print("Printing *")
    n = int(input("how many *'s?: "))
    w = int(input("break lines by how many *'s?: "))

    for i in range(n):
        print("*", end="")
        if i % w == w - 1:  # n times
            print()
    if n % w:
        print()

    # better way
    for _ in range(n // w):
        print("*" * w)

    rest = n % w
    if rest:
        print("*" * rest)

    # get input only for positive values
    print("Getting sum of integers from 1 to n")
    while True:
        n = int(input("Enter n: "))
        if n > 0:
            break

    sum = 0
    i = 1
    for i in range(1, n + 1):
        sum += i
        i += 1

    print(f"The sum of integers from 1 to n is {sum}")

    # listing the length of integer side of a rectangle
    # with area {area}
    area = int(input("Enter the area of the rectangle: "))
    for i in range(1, area + 1):
        if i * i > area:
            break
        if area % i:
            continue
        print(f"{i} x {area // i}")

    # creating n random numbers from 10 to 99
    # (stop when 13)

    n = int(input("Enter the number of random numbers: "))

    for _ in range(n):
        r = random.randint(10, 99)
        print(r, end=" ")
        if r == 13:
            print("\nProgram stopping")
            break
    else:  # if not break at the for, else is executed
        print("\nStopping creating random numbers")

    # skipping for loop
    # printing from 1 to 12, skipping 8

    for i in range(1, 13):
        if i == 8:  # this if should be computed every time
            continue
        print(i, end=" ")
    print()

    # better way
    for i in list(range(1, 8)) + list(range(9, 13)):
        print(i, end=" ")
    print()

    # getting two-digit number as input
    print("Enter two-digit integer")
    while True:
        no = int(input("Enter value: "))
        if no >= 10 and no <= 99:
            break
    print(f"The value is {no}")

    # method 2
    while True:
        no = int(input("Enter two digit number: "))
        if 10 <= no <= 99:
            break
    print(f"The value is {no}")

    # using de morgan's law
    while True:
        no = int(input("Enter two digit number: "))
        if not (no > 99 or no < 10):
            break
    print(f"The value is {no}")

    # printing multiplication table
    print("-" * 27)
    for i in range(1, 10):
        for j in range(1, 10):
            print(f"{i * j:3}", end="")
        print()
    print("-" * 27)

    # printing * in right isosceles triangle shape
    print("Printing right isosceles triangle")
    n = int(input("Enter the length of a side: "))

    for i in range(n):
        for j in range(i + 1):
            print("*", end="")
        print()

    for i in range(n):
        for _ in range(n - i - 1):
            print(" ", end="")
        for _ in range(i + 1):
            print("*", end="")
        print()

    # local variable and global variable
    n = 1
    print(f"id(1) = {id(1)}")
    print(f"id(n) = {id(n)}")
    put_id()

    for i in range(1, 101):
        print(f"i = {i:3}    id(i) = {id(i)}")
