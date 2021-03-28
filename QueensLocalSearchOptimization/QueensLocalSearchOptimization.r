# Numerical Methods and Optimization in Finance
install.packages('NMOF')

set.seed(1345797)

N <- 8

rand_position <- sample.int(N)
rand_position

data_frame <- data.frame(row = 1:N , column = rand_position)
data_frame

chessboard <- function(rand_position, queen = "Q", sep = '  '){
    n <- length(rand_position)
    row <- rep('_', n)
    
    for(i in seq_len(n)){
        row_i <- row
        row_i[rand_position[i]] <- queen
        
        cat(paste(row_i, collapse = sep))
        cat('\n')
    }
}

chessboard(rand_position)

matrix_diag <- array(NA, dim = c(N,N))
matrix_diag

for(row in  1:N)
    for(col in 1:N)
        matrix_diag[row,col] <- col - row
    

matrix_diag

matrix_rev_diag <- array(NA, dim = c(N,N))
matrix_rev_diag

for(row in  1:N)
    for(col in 1:N)
        matrix_rev_diag[row,col] <- col + row - (N + 1)
    

matrix_diag

rand_position

duplicated(rand_position)

sum(duplicated(rand_position))

duplicated(rand_position - seq_along(rand_position))

rand_position - seq_along(rand_position)

sum(duplicated(rand_position - seq_along(rand_position)))

duplicated(rand_position + seq_along(rand_position))

rand_position + seq_along(rand_position)

sum(duplicated(rand_position + seq_along(rand_position)))

num_attacks <- function(rand_position){
    sum(duplicated(rand_position))+
    sum(duplicated(rand_position - seq_along(rand_position))) +
    sum(duplicated(rand_position + seq_along(rand_position)))
}

num_attacks(rand_position)

move_one_queen <- function(rand_position){
    step <- 4
    i <- sample.int(N,1)
    
    rand_position[i] <- rand_position[1] +sample(c(1:step, -(1:step)),1)
    
    if(rand_position[i]>N)
        rand_position[i] <- 1
    else if(rand_position[i] < 1)
        rand_position[i] <- N
    
    rand_position
        
}

chessboard(rand_position)

chessboard(rand_position <- move_one_queen(rand_position))

chessboard(rand_position <- move_one_queen(rand_position))

p0 <- rep(2,N)
p0

chessboard(p0)

chessboard(rand_position <- move_one_queen(p0))

chessboard(rand_position <- move_one_queen(rand_position))







