library(hdf5r)
filename <- 'test_r.hdf5'
file <- H5File$new(filename, mode="w")

dname1 = "Integers"
data1 <- as.array(c(1:100))
data1[1] = 1000
file[[dname1]] <- data1
dset1 <- file[[dname1]]
h5attr(dset1, 'month') <- 7

dname2 = "Reals"
ndata2 = matrix(c(2.1, 3.0, 55.0, -73.0001), nrow=2, ncol=2)
file$create_dataset(dname2, ndata2)
dset2 <- file[[dname2]]
h5attr(dset2, 'year') <- 2017

file$close_all()
