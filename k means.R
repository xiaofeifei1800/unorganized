library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory


# Any results you write to the current directory are saved as output.
# Set random seed. Don't remove this line.
set.seed(1)


my_iris <- iris[-5]
species <- iris$Species

kmeans_iris=kmeans(my_iris, 3)

# Compare the actual Species to the clustering using table()
table(species, kmeans_iris$cluster)

plot(Petal.Length ~ Petal.Width, data = my_iris, col = kmeans_iris$cluster)
