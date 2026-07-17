source_all <- function(folder_path, pattern = "\\.R$") {
  # List all files in the folder that match the pattern (default: .R files)
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)

  # Loop through and source each file
  for (f in files) {
    message("Sourcing: ", f)
    source(f)
  }
}
