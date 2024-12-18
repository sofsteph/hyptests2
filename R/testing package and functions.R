# this is what we need to do to test the function in the video we will make using zoom recording.

# testing to install package
# Install devtools if not already installed
install.packages("devtools")

# Install hyptests2 from GitHub
devtools::install_github("sofsteph/hyptests2")

# load the package
require(hyptests2)
# let's see the vignette
vignette(package = "hyptests2")
vignette("hyptests2", package = "hyptests2")



# let's test the functions

# Run the t-test
# Simulated dataset
set.seed(123)
group1 <- rnorm(30, mean = 5, sd = 1)
group2 <- rnorm(30, mean = 6, sd = 1.5)

result <- t_test_2.0(group1, group2)

# Run the chi-square test
observed <- matrix(c(30, 10, 20, 40), nrow = 2, byrow = TRUE,
                   dimnames = list(c("Group 1", "Group 2"),
                                   c("Outcome A", "Outcome B")))


result <- chi_square.2.0(observed)

# Perform ANOVA
result <- anova.2.0(data = iris, formula = Sepal.Length ~ Species)

hypGadget()

vignette('hyptests2')
