library(SWOMSE)
library(usethis)


# update
fls <- list.files('data')
fls <- fls[grepl('MOM', fls)]


file.rename(file.path('data', fls), file.path('data', paste0('prev_',fls)))

fls <- list.files('data')
fls <- fls[grepl('prev_', fls)]

for (i in seq_along(fls)) {
  t <- load(file.path('data', fls[i]))
  name <- tools::file_path_sans_ext(fls[i])
  assign(name, get(t))
  do.call("use_data", list(as.name(name), overwrite = TRUE))
}


