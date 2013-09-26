# simple linear algebra in R

A <- matrix(c(1,2,3,4,5,6,7,8,-2),nrow=3)

A

# get A^(-1)
solve(A)

# Check!
A %*% solve(A)

# transpose
t(A)

# eigenvalues and eigenvectors
eigen(A)

A.eigs <- eigen(A)

A.eigs$values
A.eigs$vectors

# main diagonal
diag(A)

# trace
sum(diag(A))

# determinant
det(A)
