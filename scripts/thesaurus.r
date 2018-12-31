## The script demonstrates and explains the use of main functions in the this repository
## You can think of this as a serires of reproducible examples that summarize
## what has been learned during the development of this project

## @knitr cycle_functions

## Compute length(unique(ds)) for each of the column/variables of the dataset
Compose <- function(x, ...)
{
    lst <- list(...)
    for(i in rev(seq_along(lst)))
        x <- lst[[i]](x)
    x
}

sapply(ds, Compose, length,unique)


# ---- combine-subset ------------------------------
a <- c("A", "B", "C", "G")
b <- c("C", "D", "E", "G")
c <- c("A", "D", "F", "G")


# combine & remove common elements
union(a, b)
union(a, c)
union(b, c)

# combine & keep ONLY common elements (remove unique)
intersect(a, b)
intersect(a, c)
intersect(b, c)

# keep only elements common to all vectors
Reduce(intersect, list(a, b, c))

# keep  unique to the first vector / remove duplicates in the second vector
setdiff(a, b)
setdiff(a, c)
setdiff(b, c)