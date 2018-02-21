library(hdf5r)
filename <- 'new_r.hdf5'
file <- H5File$new(filename, mode="r+")
group <- file$open("/Folder1")
group$create_group("subfolder1")
data1 <- as.array(c(1:100))
data1[1] = 1000
group[["subfolder1/data_set_2"]] <- data1

group <- file$open("/Folder2")
group$create_group("subfolder2")
group$create_group("subfolder2/subfolder3")
group$create_group("subfolder2/subfolder3/subfoulder4")

file$close_all()
