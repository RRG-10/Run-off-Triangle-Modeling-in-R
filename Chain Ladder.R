library('ChainLadder')

data(AutoBI)
data <- AutoBI$AutoBIReportedCounts 
data

start_year <- 1988
AccidentYear <- seq(start_year, to = nrow(data)+start_year-1 , by = 1)
DevelopmentYear <- seq(1,to = ncol(data) , by = 1)
rownames(data) <- AccidentYear
colnames(data) <- DevelopmentYear

plot(data, main = "Reported Counts Triangle")

# Deterministic projection of the triangle

CL <- chainladder(data)
CL$Models

link_factors <- sapply(CL$Models , function(m) coef(m)["x"])

full_triangle <- predict(CL)
full_triangle
plot(full_triangle, main = "Completed Reported Counts Triangle")
plot(full_triangle, lattice=TRUE)

ultimates <- rowSums(full_triangle) ; ultimates
reported_to_date <- rowSums(data, na.rm = TRUE) ; reported_to_date
reserves <- ultimates- reported_to_date ; reserves
data.frame(
  AccidentYear   = rownames(data),
  Reported       = reported_to_date,
  Ultimate       = ultimates,
  Reserves  = reserves
)



