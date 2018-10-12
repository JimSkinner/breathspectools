#' Load a single breathspec file into a 1-row dataframe
#'
#' @param A single .csv file
#' @return Single-row dataframe
readBreathspecSingle <- function(file) {
  # Load metadata (throw away units stored in 2nd line)
  sample <- read.csv(file, header=TRUE, nrows=2, quote="\"", sep=";",
    fileEncoding="latin1", stringsAsFactors=FALSE)[-1,]

  # Flows are long strings; turn into vectors
  sample$Flow.Epc.1 <- I(list(as.numeric(strsplit(sample$Flow.Epc.1, " ")[[1]])))
  sample$Flow.Epc.2 <- I(list(as.numeric(strsplit(sample$Flow.Epc.2, " ")[[1]])))

  # Load breath measurement data, process into matrix with dim names
  breath <- read.csv(file, header=TRUE, quote="\"", sep=";", skip=3,
                     fileEncoding="latin1", stringsAsFactors=FALSE)
  retPerDrift <- breath$RetTime..s....DriftTime..ms.
  GCruntime   <- sub("X", "", colnames(breath)[-c(1,2)])
  breath      <- as.matrix(breath[,-c(1,2)])
  dimnames(breath) <- list("IMS drift time"=retPerDrift,
                           "GC runtime"=GCruntime)

  # Sanity check: Does chunk.sample.count & chunks.count match the breath dimensionality?
  stopifnot(ncol(breath) == sample$Chunk.sample.count)
  stopifnot(nrow(breath) == sample$Chunks.count)

  sample$Breath <- I(list(breath))

  return(sample)
}

#' Load breathspec data into a dataframe. Accepts either a vector of file
#' location, or a single file in which case every .csv is loaded.
#'
#' @param path Either a single folder, or a character vector of .csv files
#' @return A dataframe containing breathspec data
#' @examples
#' \dontrun{
#' df1 <- readBreathspec(c("breathspec1.csv", "breathspec2.csv"))
#' df2 <- readBreathspec("path/to/folder")
#' }
#' @export
readBreathspec <- function(path) {
  if (all(grepl("*.csv", path))) {
    # csv files
    files <- path
  } else if (length(path)==1) {
    # A single folder
    files <- grep("*.csv", dir(path, full.names=TRUE), value=TRUE)
  } else {
    stop("Expected: A vector of csv files OR a single folder containing csv files")
  }
  samples <- plyr::ldply(lapply(files, readBreathspecSingle))
  samples$File <- files
  return(samples)
}

# TODO: IN data-raw, load FS data.
# OR: Do I want a different package/set of scripts for specific analysis?

# TODO: Sanity checks
