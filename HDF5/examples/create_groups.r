library(hdf5r)
filename <- 'new_r.hdf5'
file <- H5File$new(filename, mode="w")
file$create_group("Folder1")
data1 <- as.array(c(1:100))
data1[1] = 1000
file[["Folder1/Integers"]] <- data1

file$create_group("Folder2")
ndata2 = matrix(c(2.1, 3.0, 55.0, -73.0001), nrow=2, ncol=2)
file$create_dataset("Folder2/Reals", ndata2)

file$close_all()

