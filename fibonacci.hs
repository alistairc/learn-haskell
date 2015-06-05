-- prints the first 20 nums from the Fibonacci sequence

fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

main = print $ map fibonacci [1..20]
    
