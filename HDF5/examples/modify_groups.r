library(hdf5r)
filename <- 'new_r.hdf5'
file <- H5File$new(filename, mode="r+")

file$ls(recursive=TRUE)

data1 <- as.array(c(1:100))
data1[1] = 1000
file[["Folder1/data_set_1"]] <- data1

file$close_all()

