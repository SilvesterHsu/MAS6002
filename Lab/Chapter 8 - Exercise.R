# %% markdown
# # Exercise A

# %% codecell
remove_low_values <- function(x, threshold) x[-which(x < threshold)]
remove_low_values(x = rnorm(n = 100))

# %% codecell
# delete all the number lower than 1
remove_low_values <- function(x, threshold) x[-which(x < threshold)]
some_threshold <- 1
remove_low_values(x = rnorm(n = 100), threshold = some_threshold)

# %% codecell
remove_low_values <- function(x, threshold) x[-which(x < threshold)]
some_threshold <- 1
remove_low_values(x = rnorm(n = 100), threshold = some_threshold)
x

# %% codecell
remove_low_values <- function(x) x[-which(x < threshold)] # note the change in arguments
remove_low_values(x = rnorm(n = 100), threshold = 1)

# %% markdown
# # Exercise B

# %% codecell
# Create matrix of ladder feet and tails
ladder_mat <- matrix(NA , nrow = 8, ncol = 2)
ladder_mat[1,] <- c(4, 14)
ladder_mat[2,] <- c(9, 31)
ladder_mat[3,] <- c(20, 38)
ladder_mat[4,] <- c(28, 84)
ladder_mat[5,] <- c(40, 59)
ladder_mat[6,] <- c(51, 67)
ladder_mat[7,] <- c(63, 81)
ladder_mat[8,] <- c(71, 91)
# Create matrix of snake heads and tails
snake_mat <- matrix(NA , nrow = 8, ncol = 2)
snake_mat[1,] <- c(17, 7)
snake_mat[2,] <- c(54, 34)
snake_mat[3,] <- c(62, 19)
snake_mat[4,] <- c(64, 60)
snake_mat[5,] <- c(87, 24)
snake_mat[6,] <- c(93, 73)
snake_mat[7,] <- c(95, 75)
snake_mat[8,] <- c(99, 78)
# Function to move the counter
make_move <- function(start_sq){
  finish_sq <- start_sq + sample(x = 1:6, size = 1)
}
# Function that moves the player up a ladder if they land on the ladder foot
climb_ladder <- function(ladder_foot, ladder_mat){
  row <- which(ladder_mat[, 1] == ladder_foot)
  ladder_mat[row, 2]
}
# Function that moves a player down a snake if they land on a snake head
descend_snake <- function(snake_head, snake_mat){
  row <- which(snake_mat[, 1] == snake_head)
  snake_mat[row, 2]
}
# Function to play the game until the player reaches 100 or more.
# The function returns the number of moves taken.
# If `to_print = T` then the function prints the squares visited in each move
count_num_moves <- function(snake_mat, ladder_mat, to_print){
  num_moves <- 0
  start_sq <- 1
  finished_game = FALSE
  while(finished_game == FALSE){
    if(to_print == T) cat("start at square = ", start_sq, "\t")
    num_moves <- num_moves + 1
    next_sq <- make_move(start = start_sq)
    if(to_print == T) cat("move to square= ", next_sq, "\n")
    if(any(ladder_mat[, 1] == next_sq)) {
      next_sq <- climb_ladder(next_sq, ladder_mat)
      if(to_print == T) cat("climb ladder to",next_sq, "\n")
    }
    if(any(snake_mat[, 1] == next_sq)) {
      next_sq <- descend_snake(next_sq, snake_mat)
      if(to_print == T) cat("descend snake to", next_sq, "\n")
    }
    if(next_sq >= 100) finished_game = TRUE else{
      start_sq <- next_sq
    }
  }
  return(num_moves)
}
count_num_moves(snake_mat,ladder_mat,to_print=T)

Rprof(tmp<-tempfile())
count_num_moves(snake_mat,ladder_mat,to_print=T)
Rprof()
summaryRprof(tmp)$by.self
