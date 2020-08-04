
# -------------------------------------
# Define project path to read own libraries
# --------------------------------------

.libPaths(c("/projappl/project_2003256/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
                       

# Install the package
#install.packages("ggspatial", lib = libpath)