library(readr)
data <- read_csv("data/data.csv")


filesize <- file.info("data/data.csv")
filesize <- filesize$size/1000000

how_many <- round(filesize/50,0)


total_row <- nrow(data)

row_chunks <- round(total_row/how_many,0)+1

chunks = split(data,sample(rep(1:how_many,row_chunks)))  ## 5 Chunks of 30 lines each

for (i in 1:length(chunks)) {
  splitted <- chunks[[i]]
  write.csv(splitted, paste0("data_partition/data", i, ".csv"))
}