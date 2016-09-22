#Matrices 
A=matrix(
	c(2,4,3,1,5,7), # the elements of the matrix
	nrow=2, #number of rows
	ncol=3, #number of columns
	byrow=T) #fill matrix by rows
A #print the matrix

##Calling elements
#An element at the mth row, nth column of A can be accessed by the expresseion A[m,n].
A[2,3] #element at 2nd row, 3rd column
#the entire mth row can be extracted as A[m,]
A[2,] #the second row
#can call an entire column
A[,3]
#call multiple rows or columns
A[,c(1,3)]
#assign names to the rows and columns of the matrix
dimnames(A)=list(
	c("row1","row2"),#row names
	c("col1","col2","col3")) #column names
A
#can call by names
A["row2","col3"]

#can transpose a matrix
B<-t(A) #transpose of A
B
C<-t(B)
#combining the columns of A and C with cbind
cbind(C,A)
#combining the rows of A and C with R bind
rbind(A,C)

#matrix algebra: http://www.statmethods.net/advstats/matrix.html

#Matrix Multiplication
A%*%B
#Element wisemultiplication
A*C
#Outer product
A%o%B
#square matrix
z=matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=T)
z
#inverse of a matrix
solve(z)

#reduced row echelon form 
install.packages("editrules")
library(editrules)
echelon(A)
#look more into this packge http://cran.r-project.org/web/packages/editrules/editrules.pdf

