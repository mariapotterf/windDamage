

# Rename all files

# Change extensions of all files

df.files = list.files("C:/MyTemp/avohaakut_db/solutions",
                      full.names = TRUE) #

new.names<- gsub(".csv", "_", df.files)
new.files <- paste(new.names, ".csv", sep = "")
file.rename(df.files, new.files)
