library(hdf5r)
filename <- 'new_r.hdf5'
file <- H5File$new(filename, mode="r")


dset1 <- file[["Integers"]]
d1 <- dset1[]
d1

dset2 <- file[["Reals"]]
dset2[,]
h5attr(dset2, "year")


file$close_all()

