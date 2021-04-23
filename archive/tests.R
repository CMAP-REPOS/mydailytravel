source("R/helper_fns.R")

# Create a dummy data set to run the helper_fns calculation on. Verify this
# works after any changes to the function.
dummy_trips <- 
  tibble(color = c("yellow",rep("blue",18),"red"),
         age = rep(c(10,15,20,25,30),4),
         weight = rep(c(1,2,3,4,5),4))

solution <- 
  tibble(color = c(rep("blue",5),"red","yellow"),
         age = c(10,15,20,25,30,30,10),
         breakdown_total = c(3,8,12,16,15,5,1),
         total = c(4,8,12,16,20,20,4),
         total_n = 4,
         breakdown_n = c(3,4,4,4,3,1,1),
         pct = c(.75,1,1,1,.75,.25,.25),
         survey = "test")

pct_calculator(dummy_trips,
               weight = "weight",
               breakdown_by = "color",
               second_breakdown = "age",
               survey = "test"
               ) == solution
