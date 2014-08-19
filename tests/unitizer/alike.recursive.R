library(alike)

lst <- list(list(1L, "a"), TRUE, data.frame(nums=1L:3L, alfs=letters[5L:7L]))

index_structure(lst)

(term.indices <- index_structure(lst))
(all.indices <- index_structure(lst, terminal.nodes.only=FALSE))

alike(term.indices, all.indices)

lst <-   list(list(list(1:3, "a"), "a"), 1:3)
lst.2 <- list(list(list(        ),   1), "a")          # empty lists considered terminal
lst.3 <- list(list(list(1:3, "a"), "a"), 1:3, "hello")

index_structure(lst)
index_structure(lst.2)
index_structure(lst.3)

alike(index_structure(lst.2), index_structure(lst))    # TRUE
alike(index_structure(lst), index_structure(lst.2))    # FALSE because `lst` is reference and has more detail
alike(index_structure(lst), index_structure(lst.3))
alike(index_structure(lst.3), index_structure(lst.2))

index_structure(list(list(1, 2, list(1:5, NULL)), data.frame(1:3, letters[1:3])))
index_structure(list(list(1, 2, list(1:5, NULL)), data.frame(1:3, letters[1:3])), terminal.nodes.only=FALSE)
