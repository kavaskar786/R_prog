#1.create two matrix
a=matrix(c(1:9),3,3)
b=matrix(c(11:19),3,3)

#sum
c=a+b
print(c)

#diff
c=a-b
print(c)

#* with 2
c=a*2
print(c)

#calculate the product of 2 matrix
c=a %*%b
print(c)

#transpose
print(t(c))

#determinant
print(det(c))

a1 <- c(3, 2, 5) 
a2 <- c(2, 3, 2) 
a3 <- c(5, 2, 4) 

# bind the three vectors into a matrix  
# using rbind() which is basically 
# row-wise binding. 
A <- rbind(a1, a2, a3)
# to calculate the inverse. 
T1 <- solve(A) 

# print the inverse of the matrix. 
print(T1) 

#inverse
#d=solve(b)
#print(d)

if (all(t(A)==solve(A))){
  print("its orthogonal")
}

for (i in c(1:3)){
  for (j in c(1:3)){
    c[i,j]=a[i,j]**2
  }
}
print(c)

# Assuming matrix_B is defined
mean_B <- mean(b)
print(mean_B)

# Assuming matrix_A is defined
column_sums <- colSums(a)
print(column_sums)

# Assuming matrix_B is defined
row_means_B <- rowMeans(b)
print(row_means_B)

# Assuming matrix_A is defined
second_row_A <- a[2, ]
print(second_row_A)

# Assuming matrix_B is defined
third_column_B <- b[, 3]
print(third_column_B)

