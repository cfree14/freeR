
#' Convert RDS file to CSV file
#'
#' Converts an RDS file containing an R dataframe to a CSV.
#'
#' @param file RDS file name including path
#' @return A CSV file in the same folder and with the same name as the RDS file
#' @export
rds2csv <- function(file) {

  data <- readRDS(file)
  outfile <- gsub(".Rds", ".csv", file)
  write.csv(data, file=outfile, row.names = F)

}





