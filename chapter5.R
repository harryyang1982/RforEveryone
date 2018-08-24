# 5.1 data.frames

x <- 10:1
y <- -4:5
q <- c("Hockey", "Football", "Baseball", "Curling", "Rugby",
       "Lacrosse", "Basketball", "Tennis", "Cricket", "Soccer")
theDF <- data.frame(x, y, q)

theDF

theDF <- data.frame(First=x, Second=y, Sport=q)
theDF

nrow(theDF)
ncol(theDF)
dim(theDF)

names(theDF)
names(theDF)[3]

rownames(theDF)
rownames(theDF) <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten")
rownames(theDF)

head(theDF)
head(theDF, n=7)
tail(theDF)

class(theDF)

theDF$Sport

theDF[3, 2]

theDF[3, 2:3]
theDF[c(3, 5), 2]

theDF[c(3, 5), 2:3]

theDF[, 3]

theDF[, 2:3]

theDF[2, ]

theDF[2:4, ]

theDF[, c("First", "Sport")]
theDF[, "Sport"]

class(theDF[, "Sport"])

theDF["Sport"]

class(theDF["Sport"])

theDF[["Sport"]]
class(theDF[["Sport"]])

theDF[, "Sport", drop=FALSE]
class(theDF[, "Sport", drop=FALSE])

theDF[, 3, drop=FALSE]
class(theDF[, 3, drop=FALSE])

newFactor <- factor(c("Pennsylvania", "New York", "New Jersey",
                      "New York", "Tennessee", "Massachusetts",
                      "Pennsylvania", "New York"))
model.matrix(~ newFactor - 1)

# 5.2 Lists

list(1, 2, 3)
list(c(1, 2, 3))

(list3 <- list(c(1, 2, 3), 3:7))

list(theDF, 1:10)

list5 <- list(theDF, 1:10, list3)
list5

names(list5)

names(list5) <- c("data.frame", "vector", "list")
names(list5)

list5

list6 <- list(TheDataFrame = theDF, TheVector = 1:10, TheList = list3)
list6
names(list6)

(emptyList <- vector(mode = "list", length = 4))

list5[[1]]
list5[1]

list5[["data.frame"]]
list5["data.frame"]

list5[[1]]$Sport

list5[[1]][, "Second"]
list5[[1]][, "Second", drop = FALSE]

length(list5)
list5[[4]] <- 2
length(list5)

list5
list5[["NewElement"]] <- 3:6
list5
length(list5)

names(list5)

list5

# 5.3 Matrices

A <- matrix(1:10, nrow = 5)
B <- matrix(21:30, nrow = 5)
C <- matrix(21:40, nrow = 2)
A
B
C

nrow(A)
ncol(A)
dim(A)

A + B
A * B
A == B
all(A == B)

A %*% t(B)
t(A) %*% B

colnames(A)
rownames(A)

colnames(A) <- c("Left", "Right")
rownames(A) <- c("1st", "2nd", "3rd", "4th", "5th")

colnames(B)
rownames(B)

colnames(B) <- c("First", "Second")
rownames(B) <- c("One", "Two", "Three", "Four", "Five")

colnames(C)

colnames(C) <- LETTERS[1:10]
rownames(C) <- c("Top", "Bottom")

A
t(A)
A %*% C

# 5.4 Arrays

theArray <- array(1:12, dim = c(2, 3, 2))
theArray

theArray[1, ,]
theArray[1, , 1]
theArray[, , 1]
