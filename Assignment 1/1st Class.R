Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre8')
install.packages('rJava', repos='http://www.rforge.net/')
Sys.getenv("JAVA_HOME")
options(java.home="C:\\Program Files\\Java\\jre7\\")

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_31/")


#Vectors
a <- c(1,2,4.5,8,-3,5) #numeric vector
b <- c("one","two","three")  #character vector
c <- c(TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE)  #logical vector


#matrix

cells <- c(1,26,24,68)
rnames <- c("R1","R2")
cnames <- c("C1","C2")
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE, dimnames = list(rnames, cnames))

x<-matrix(1:20, nrow = 5, ncol = 4)
x[,4] # 4th column of matrix
x[3,] # 3rd row of matrix
x[2:4,1:3] # rows 2,3,4 of column 1,2,3
tyoeof(x)
mode(x)
class(x)


#frames

d <- c(1,2,3,4)
e <- c("red","white","red", NA)
f <- C(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
names(mydata) <- c("ID","Color","Passed") #Variable names
mydata

#lists

#examples of lists with 4 components -
# a string, a numeric vector, a matrix and a scalar
w <- list(name)

#Arrays
#Create an Array
arr <- array(c('green','yellow'),dim = c(3,3,2))
print(arr)
