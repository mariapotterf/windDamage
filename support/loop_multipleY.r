

# Loop over multiple Y variables
# and create multiple plots

set.seed(5)
df <- data.frame(x = rep(c(1:5), 2),
                 y1 = rnorm(10)*3+2,
                 y2 = rnorm(10),
                 y3 = 1:10,
                 y4  = 10:1,
                 group = rep(c("a", "b"), each = 5))  


# vector of my Y
my.s<-c("y1", "y2", 'y3', 'y4')

# Create a function



Plotfunction <- function(y){my.plot <- 
  ggplot(df, aes_string(x = "x",
                        y = y,
                        group = "group",
                        color = "group")) +
  geom_line()}



do.call("grid.arrange",
        c(lapply(my.s, Plotfunction), ncol = length(my.s)))


