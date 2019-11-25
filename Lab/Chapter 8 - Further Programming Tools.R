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
