#EXERCISE

mat = matrix(1:16, nrow = 4)
rowname <- c("row1","row2", "row3", "row4")
colname <- c("col1","col2", "col3", "col4")
dimnames(mat)<- list(rowname, colname)
mat

vector  = c(mat[1,1], mat[2,2], mat[3,3], mat[4,4])
a = diag(4)*mat
a

mat2 = mat[c("row1","row2"),]
mat2

data = read.table("http://www.oikostat.ch/data/parusater.txt", header=TRUE)
data
length(data)
data$age
