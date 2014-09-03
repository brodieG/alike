library(alike)

lst <-   list(list( 1,  2), list( 3, list( 4, list( 5, list(6, 6.1, 6.2)))))
lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61          )))))

alike2(lst, lst.2)

lst.3 <- lst.2
lst.3[[2]][[2]][[2]][[2]] <- matrix(1:9, nrow=3)

alike2(lst, lst.3)
alike2(1:10, "hello")
