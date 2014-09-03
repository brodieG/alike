library(alike)

lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61          )))))

alike2(lst, lst.2)     # length mismatch

lst.3 <- lst.2
lst.3[[2]][[2]][[2]][[2]] <- matrix(1:9, nrow=3)

alike2(lst, lst.3)     # object type mismatch
alike2(1:10, "hello")  # object type mismatch, no dive

alike2(lst, lst)       # obvious match

lst.4 <- lst
lst.4[[2]][[2]] <- list()

alike2(lst.4, lst)     # should match
alike2(lst, lst.4)     # should not match because template has more detail 
