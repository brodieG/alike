library(vectools)
library(alike)

lst <- list(list(1L, "a"), TRUE, data.frame(nums=1L:3L, alfs=letters[5L:7L]))

index_structure(lst)

term.indices <- index_structure(lst)
all.indices <- index_structure(lst, terminal.nodes.only=FALSE)

alike(term.indices, all.indices)

lst <-   list(list(list(1:3, "a"), "a"), 1:3)
lst.2 <- list(list(list(        ),   1), "a")
lst.3 <- list(list(list(1:3, "a"), "a"), 1:3, "hello")

alike(index_structure(lst.2), index_structure(lst))
alike(index_structure(lst), index_structure(lst.2))
alike(index_structure(lst), index_structure(lst.3))
alike(index_structure(lst.3), index_structure(lst.2))
