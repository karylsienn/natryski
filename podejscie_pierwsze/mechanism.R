
for(j in seq_len(20)) {
  source("fig_create.R")
  source("fig_transform.R")
  source("triangles.R")
  readr::write_csv(wielokat, file.path("wielokaty", sprintf("wielokat%00d.csv", j)))
  readr::write_csv(circles, file.path("circles", sprintf("circles%00d.csv", j)))
}

