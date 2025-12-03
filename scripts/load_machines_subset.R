load_machines_subset <- function() {
  library(rrcov)
  data("machines")
  

  i1 <- which(rownames(machines) == "hp-3000/64")
  i2 <- which(rownames(machines) == "ibm-4331-2")
  
  machines[i1:i2, ]
}
