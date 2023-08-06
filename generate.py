from random import randint
from glob import glob

print("Generation of a matrix file")

files = glob("matrices/*")

i = 1

while "matrices/matrix{}.txt".format(i) in files:
	i += 1

filename = "matrices/matrix{}.txt".format(i)
n = int(input("Size of the matrix : "))

with open(filename, "w") as file:
	for i in range(n):
		file.write(" ".join([str(randint(-50, 50)) for _ in range(n)]))
		file.write("\n")

print("The file {} was created".format(filename))
