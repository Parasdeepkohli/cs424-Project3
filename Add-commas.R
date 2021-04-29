addCommas <- function(n){
  
  char <- as.character(n)
  for (i in 1:length(char)){
    c <- char[i]
    if (nchar(c) == 10){
      head <- substr(c, 1,1)
      tail1 <- substr(c, 2, 4)
      tail2 <- substr(c, 5, 7)
      tail3 <- substr(c, 8, 10)
      char[i] <- paste0(head, ',', tail1, ',', tail2, ',', tail3)
    }
    else if (nchar(c) == 9){
      head <- substr(c, 1, 3)
      tail1 <- substr(c, 4, 6)
      tail2 <- substr(c, 7,9)
      char[i] <- paste0(head, ',', tail1, ',', tail2)
    }
    else if (nchar(c) == 8){
      head <- substr(c, 1, 2)
      tail1 <- substr(c, 3, 5)
      tail2 <- substr(c, 6, 8)
      char[i] <- paste0(head, ',', tail1, ',', tail2)
    }
    else if (nchar(c) == 7){
      head <- substr(c, 1, 1)
      tail1 <- substr(c, 2, 4)
      tail2 <- substr(c, 5, 7)
      char[i] <- paste0(head, ',', tail1, ',', tail2)
    }
    else{
      head <- substr(c, 1, 3)
      tail <- substr(c, 4, 6)
      char[i] <- paste0(head, ',', tail)
    }
    
  }
  return(as.vector(char))
}