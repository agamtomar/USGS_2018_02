library(hdf5r)
filename <- 'new_r.hdf5'
file <- H5File$new(filename, mode="r+")

file$ls(recursive=TRUE)

dset1 <- file[["Folder1/data_set_1"]]
d1 <- dset1[]
d1

file$close_all()

