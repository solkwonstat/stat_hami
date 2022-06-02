"""Chapter 1. Algorithm Basics"""


def max3(a, b, c):
    """returns maximum value among a, b, c"""
    maximum = a
    if b > maximum:
        maximum = b
    if c > maximum:
        maximum = c
    return maximum


def med3(a, b, c):
    """returns median value among a, b, c"""
    if a >= b:
        if b >= c:
            return b
        elif c >= a:
            return a
        else:
            return c
    elif a > c:
        return a
    elif b > c:
        return c
    else:
        return b


if __name__ == "__main__":
    # print("Type your name.: ", end="")
    # name = input()
    # print(f"Hi, {name}.")
    print(f"max3(3, 2, 1) = {max3(3, 2, 1)}")
    print(f"max3(3, 2, 2) = {max3(3, 2, 2)}")
    print(f"max3(3, 1, 2) = {max3(3, 1, 2)}")
    print(f"max3(3, 2, 3) = {max3(3, 2, 3)}")

    print(f"med3(3, 2, 1) = {med3(3, 2, 1)}")
    print(f"med3(3, 2, 2) = {med3(3, 2, 2)}")
