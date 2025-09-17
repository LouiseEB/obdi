# List all R and Rmd files you want to include
files <- list.files(getwd(), pattern = "\\.(R|Rmd|md)$", recursive = TRUE)

# Concatenate into a single text file
all_code <- unlist(lapply(files, function(f) {
    c(paste0("\n\n### FILE: ", f, " ###\n\n"), readLines(f))
}))

# Write to a master text file
writeLines(all_code, "manuscript_code_bundle.txt")
