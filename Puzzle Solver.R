#Specify target
target = c("Jun", 5)

#Game setup
board <- matrix(rep(0, 49), ncol = 7, nrow = 7)
board[1:2,7] <- NA
board[7,4:7] <- NA
labeled_board <- matrix(c(month.abb[1:6], NA, month.abb[7:12], NA, 1:31, rep(NA, 4)), ncol = 7, nrow = 7, byrow = TRUE)

#Remove targets from the board
board[labeled_board == target[1]] <- NA
board[labeled_board == target[2]] <- NA

print(labeled_board)
print(board)

#Create pieces
piece1 <- matrix(rep(1,6), ncol = 2, byrow = TRUE)
piece2 <- matrix(c(2,2,2,0,0,2,0,0,2), ncol = 3, byrow = TRUE)
piece3 <- matrix(c(3,3,3,0,3,3), ncol = 2, byrow = TRUE)
piece4 <- matrix(c(4,0,4,0,4,0,4,4), ncol = 2, byrow = TRUE)
piece5 <- matrix(c(5,0,5,0,5,5,5,0), ncol = 2, byrow = TRUE)
piece6 <- matrix(c(6,0,6,6,0,6,0,6), ncol = 2, byrow = TRUE)
piece7 <- matrix(c(7,7,7,7,0,7), ncol = 2, byrow = TRUE)
piece8 <- matrix(c(0,8,8,0,8,0,8,8,0), ncol = 3, byrow = TRUE)
piece_list <- list(piece1, piece2, piece3, piece4, piece5, piece6, piece7, piece8)
print(piece_list)

rotate <- function(x) t(apply(x, 2, rev))
place_piece <- function(piece, board){
  
  #Orientations of the piece
  orient1 <- piece
  orient2 <- rotate(orient1)
  orient3 <- rotate(orient2)
  orient4 <- rotate(orient3)
  orient5 <- t(orient1)
  orient6 <- rotate(orient5)
  orient7 <- rotate(orient6)
  orient8 <- rotate(orient7)
  
  #Unique orientations of the piece
  pieces_list <- unique(list(orient1, orient2, orient3, orient4, orient5, orient6, orient7, orient8))
  
  #Placements
  placements_list <- list()
  
  #Brute force place all possible places for that piece
  counter <- 1
  
  for(p in 1:length(pieces_list)){
    
    piece <- pieces_list[[p]]
    
    for(i in 1:(dim(board)[1]-dim(piece)[1]+1)){
      for(j in 1:(dim(board)[2]-dim(piece)[2]+1)){
        board_temp <- board
        
        rows <- i:(i+dim(piece)[1]-1)
        columns <- j:(j+dim(piece)[2]-1)
        
        fit_check <- (sum(is.na(board_temp[rows, columns][piece != 0])) +
                        sum(board_temp[rows, columns][piece != 0], na.rm = TRUE)) == 0
        
        if(fit_check){
          board_temp[rows, columns][piece != 0] <- piece[piece != 0]
          placements_list[[counter]] <- board_temp
          counter = counter + 1
        }
      }
    }
  }
  
  return(placements_list)
  
}

{i = 1
j = 1
k = 1
l = 1
m = 1
n = 1
o = 1
p = 1
solution_counter = 0
stop = FALSE}

solutions <- list()

placements1 <- place_piece(piece1, board)

for(p1 in placements1){print(i)
  placements2 <- place_piece(piece2, p1)
  i = i + 1
  if(stop){break}
  
  for(p2 in placements2){
    placements3 <- place_piece(piece3, p2)
    j = j + 1
    if(stop){break}
    
    for(p3 in placements3){
      placements4 <- place_piece(piece4, p3)
      k = k + 1
      if(stop){break}
      
      for(p4 in placements4){
        placements5 <- place_piece(piece5, p4)
        l = l + 1
        if(stop){break}
        
        for(p5 in placements5){
          placements6 <- place_piece(piece6, p5)
          m = m + 1
          if(stop){break}
          
          for(p6 in placements6){
            placements7 <- place_piece(piece7, p6)
            n = n + 1
            if(stop){break}
            
            for(p7 in placements7){
              placements8 <- place_piece(piece8, p7)
              o = o + 1
              if(stop){break}
              
              for(f in placements8){
                if(min(f, na.rm = TRUE) > 0){
                  solution_counter = solution_counter + 1
                  solutions[[solution_counter]] = f
                  print(paste("Solution:", solution_counter))
                  print(f)
                  }
                if(stop){break}
                #stop = min(f, na.rm = TRUE) > 0
                if(stop){break}
              }
            }
          }
        }
      }
    }
  }
}

print(solutions)

# #Get board edge
# row_board <- row(board)
# col_board <- col(board)
# edges <- matrix(rep(FALSE, 49), ncol = 7, nrow = 7)
# edges[is.na(board)] <- NA
# row_board[is.na(board)] <- NA
# col_board[is.na(board)] <- NA
# 
# for(col in 1:ncol(board)){
#   row_max <- apply(row_board, 2, max, na.rm = TRUE)[col]
#   edges[row_max, col] <- TRUE
#   row_min <- apply(row_board, 2, min, na.rm = TRUE)[col]
#   edges[row_min, col] <- TRUE
# }
# 
# for(row in 1:nrow(board)){
#   col_max <- apply(col_board, 1, max, na.rm = TRUE)[row]
#   edges[row, col_max] <- TRUE
#   col_min <- apply(col_board, 1, min, na.rm = TRUE)[row]
#   edges[row, col_min] <- TRUE
# }