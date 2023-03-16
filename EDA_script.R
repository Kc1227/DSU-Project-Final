library(R6)

interpret_hand <- function(info){
  order = c('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
  
  value_1 <- substr(info[2], 1, 1)
  suit_1 <- substr(info[2], 2, 2)
  value_2 <- substr(info[2], 4, 4)
  suit_2 <- substr(info[2], 5, 5)
  
  if(which(order %in% value_1) > which(order %in% value_2)){
    temp_value <- value_1
    temp_suit <- suit_1
    value_1 <- value_2
    suit_1 <- suit_2
    value_2 <- temp_value
    suit_2 <- temp_suit
  }
  
  if(value_1 == value_2){
    hand <- paste(value_1, value_2, sep = "")
  } else if (suit_1 == suit_2){
    hand <- paste(value_1, value_2, "s", sep = "")
  } else {
    hand <- paste(value_1, value_2, "o", sep = "")
  }
  
  list(hand, paste(value_1, suit_1, sep = ""), paste(value_2, suit_2, sep = ""))
}

Hand <- R6Class(
  classname = "Hand",
  public = list(
    hand_number = NA,
    table_number = NA,
    date = NA,
    time = NA,
    UTG = NULL,
    UTG1 = NULL,
    UTG2 = NULL,
    BTN = NULL,
    SB = NULL,
    BB = NULL,
    Hero = NULL,
    Total_pot = 0,
    Winnable_pot = 0,
    Rake = 0,
    Flop = NA,
    Turn = NA,
    River = NA,
    players_in_hand = list(),
    winners = list(),
    missing_SB = FALSE,
    num_players = 0,
    opener = NA,
    initialize = function(text){
      
    ## Initialize Background info
      self$hand_number <- str_match(text, "Hand #(\\d{10})")[2]
      self$table_number <- str_match(text, "TBL#(\\d{8})")[2]
      self$date <- str_match(text, "(\\d{4}-\\d{2}-\\d{2})")[2]
      self$time <- str_match(text, "(\\d{2}:\\d{2}:\\d{2})")[2]
      
    ## Initialize Players, Set Hero
      players <- str_match_all(text, "(?:Seat [:digit:]: )([A-Za-z1-2\\+\\s]+[A-Za-z1-2])")[[1]][,2]

      
      ## Fix Seat sit out errors
      for(i in players){
        posit_temp <- i
        if(i == "UTG+1") posit_temp <- "UTG\\+1"
        if(i == "UTG+2") posit_temp <- "UTG\\+2"
        sitting_out <- str_match(str_match(text, "[^\\*]+\\*"), paste(posit_temp, " : Seat sit out", sep = ""))
        sitting_out_wait <- str_match(str_match(text, "[^\\*]+\\*"), paste(posit_temp, " : Sitout", sep = ""))
        if(!is.na(sitting_out) | !is.na(sitting_out_wait)){
          players <- players[players != i]
        }
      }

      self$num_players <- length(players)
      
      
      #print(str_match_all(text, "Seat [:digit:]")[[1]])
      
      ## Fix No Dealer or Missing Player Errors
        if(self$num_players == 5 & "Small Blind" %in% players & "Big Blind" %in% players & !("Dealer" %in% players)){
          text <- str_replace_all(text, "Dealer", "OUT")
          text <- str_replace_all(text, "UTG\\+2", "Dealer")
        }
        if(self$num_players == 4 & "Small Blind" %in% players & "Big Blind" %in% players & !("Dealer" %in% players)){
          text <- str_replace_all(text, "Dealer", "OUT")
          text <- str_replace_all(text, "UTG\\+1", "Dealer")
        }
        if(self$num_players == 3 & "Small Blind" %in% players & "Big Blind" %in% players & !("Dealer" %in% players)){
          text <- str_replace_all(text, "Dealer", "OUT")
          text <- str_replace_all(text, "UTG\\s", "Dealer ")
        }
        if(self$num_players == 5 & "Small Blind" %in% players & "Big Blind" %in% players & "Dealer" %in% players
           & "UTG+2" %in% players & !("UTG+1" %in% players)){
          text <- str_replace_all(text, "UTG\\+1", "OUT")
          text <- str_replace_all(text, "UTG\\+2", "UTG\\+1")
        }
        if(self$num_players == 5 & "Small Blind" %in% players & "Big Blind" %in% players & "Dealer" %in% players
           & "UTG+2" %in% players & !("UTG" %in% players)){
          text <- str_replace_all(text, "UTG ", "OUT ")
          text <- str_replace_all(text, "UTG\\+1", "UTG")
          text <- str_replace_all(text, "UTG\\+2", "UTG\\+1")
        }
    
      
      if(self$num_players >= 1){
        info <- str_match(text, "(Big Blind) (\\[ME\\] )?\\(\\$([0-9\\.]{1,6})")
        self$BB <- Player$new(position = info[2], chips = info[4])
        self$players_in_hand[[self$num_players]] <- self$BB
        if(!is.na(info[3])){
          self$BB$hero <- TRUE
          self$Hero <- self$BB
          }
        if(self$num_players >= 2){
          info <- str_match(text, "(Dealer) (\\[ME\\] )?\\(\\$([0-9\\.]{1,6})")
          self$BTN <- Player$new(position = info[2], chips = info[4])
          self$players_in_hand[[self$num_players-1]] <- self$BTN
          if(!is.na(info[3])){
            self$BTN$hero <- TRUE
            self$Hero <- self$BTN
          }
          if(self$num_players >= 3){
            info <- str_match(text, "(Small Blind) (\\[ME\\] )?\\(\\$([0-9\\.]{1,6})")
            if("Small Blind" %in% players){
              self$SB <- Player$new(position = info[2], chips = info[4])
              self$players_in_hand[[self$num_players-1]] <- self$SB
              self$players_in_hand[[self$num_players-2]] <- self$BTN
              if(!is.na(info[3])){
                self$SB$hero <- TRUE
                self$Hero <- self$SB
              }
            } else {
              self$missing_SB <- TRUE
            }

            if(self$num_players + self$missing_SB >= 4){
              info <- str_match(text, "(UTG) (\\[ME\\] )?\\(\\$([0-9\\.]{1,6})")
              self$UTG <- Player$new(position = info[2], chips = info[4])
              self$players_in_hand[[self$num_players + self$missing_SB - 3]] <- self$UTG
              if(!is.na(info[3])){
                self$UTG$hero <- TRUE
                self$Hero <- self$UTG
              }
              if(self$num_players + self$missing_SB >= 5){
                info <- str_match(text, "(UTG\\+1) (\\[ME\\] )?\\(\\$([0-9\\.]{1,6})")
                self$UTG1 <- Player$new(position = info[2], chips = info[4])
                self$players_in_hand[[self$num_players + self$missing_SB - 3]] <- self$UTG1
                self$players_in_hand[[self$num_players + self$missing_SB - 4]] <- self$UTG
                if(!is.na(info[3])){
                  self$UTG1$hero <- TRUE
                  self$Hero <- self$UTG1
                }
                if(self$num_players + self$missing_SB >= 6){
                  info <- str_match(text, "(UTG\\+2) (\\[ME\\] )?\\(\\$([0-9\\.]{1,6})")
                  self$UTG2 <- Player$new(position = info[2], chips = info[4])
                  self$players_in_hand[[self$num_players + self$missing_SB - 3]] <- self$UTG2
                  self$players_in_hand[[self$num_players + self$missing_SB - 4]] <- self$UTG1
                  self$players_in_hand[[self$num_players + self$missing_SB - 5]] <- self$UTG
                  if(!is.na(info[3])){
                    self$UTG2$hero <- TRUE
                    self$Hero <- self$UTG2
                  }
          }}}}}}
        
        
        
      

      ## Initialize Hands
      if(!is.null(self$UTG)){
        info <- str_match(text, "UTG (?: \\[ME\\] )?: Card dealt to a spot \\[([:alnum:]{2} [:alnum:]{2})\\]")
        L <- interpret_hand(info)
        self$UTG$hand <- L[[1]]
        self$UTG$card1 <- L[[2]]
        self$UTG$card2 <- L[[3]]
      }  
      if(!is.null(self$UTG1)){
        info <- str_match(text, "UTG\\+1 (?: \\[ME\\] )?: Card dealt to a spot \\[([:alnum:]{2} [:alnum:]{2})\\]")
        L <- interpret_hand(info)
        self$UTG1$hand <- L[[1]]
        self$UTG1$card1 <- L[[2]]
        self$UTG1$card2 <- L[[3]]
      }  
      if(!is.null(self$UTG2)){
        info <- str_match(text, "UTG\\+2 (?: \\[ME\\] )?: Card dealt to a spot \\[([:alnum:]{2} [:alnum:]{2})\\]")
        L <- interpret_hand(info)
        self$UTG2$hand <- L[[1]]
        self$UTG2$card1 <- L[[2]]
        self$UTG2$card2 <- L[[3]]
      } 
      if(!is.null(self$BTN)){
        info <- str_match(text, "Dealer (?: \\[ME\\] )?: Card dealt to a spot \\[([:alnum:]{2} [:alnum:]{2})\\]")
        L <- interpret_hand(info)
        self$BTN$hand <- L[[1]]
        self$BTN$card1 <- L[[2]]
        self$BTN$card2 <- L[[3]]
      }
      if(!is.null(self$SB)){
        info <- str_match(text, "Small Blind (?: \\[ME\\] )?: Card dealt to a spot \\[([:alnum:]{2} [:alnum:]{2})\\]")
        L <- interpret_hand(info)
        self$SB$hand <- L[[1]]
        self$SB$card1 <- L[[2]]
        self$SB$card2 <- L[[3]]
      } 
      if(!is.null(self$BB)){
        info <- str_match(text, "Big Blind (?: \\[ME\\] )?: Card dealt to a spot \\[([:alnum:]{2} [:alnum:]{2})\\]")
        L <- interpret_hand(info)
        self$BB$hand <- L[[1]]
        self$BB$card1 <- L[[2]]
        self$BB$card2 <- L[[3]]
      } 
      
      ## Determine limpers
      preflop_action <- str_match(text, "\\*\\*\\* HOLE CARDS \\*\\*\\*([^\\*]+)\\*")[2]
      limpers <- str_match_all(preflop_action, regex("(^[A-Za-z0-9\\+\\s]+) (?: \\[ME\\] )?: (?:Calls \\$0\\.[12]5|Checks)", multiline = TRUE))[[1]][,2]
      if(length(limpers) != 0){
        for(i in seq_len(self$num_players)){
          if(self$players_in_hand[[i]]$position %in% limpers){
            self$players_in_hand[[i]]$preflop <- "Limped"
          }
        }
      }
      ## Determine opener
      opener <- str_match(preflop_action, "([^\r\n\\[\\]]{3,11}) (?: \\[ME\\] )?: (?:Raises|All\\-in)")[2]
      if(!is.na(opener)){
        for(i in seq_len(self$num_players)){
          if(self$players_in_hand[[i]]$position == opener){
            self$players_in_hand[[i]]$preflop <- "Opened"
            if(i != 1){
              for(j in 1:(i-1)){
                if(self$players_in_hand[[j]]$preflop != "Limped"){
                  self$players_in_hand[[j]]$preflop <- "Folded before open"
                }
              }
            }
            if(i != self$num_players){
              for(j in (i+1):self$num_players){
                posit <- self$players_in_hand[[j]]$position
                if(posit == "UTG+1"){posit <- "UTG\\+1"}
                if(posit == "UTG+2"){posit <- "UTG\\+2"}
                if(!is.na(str_match(preflop_action, paste(posit, " (?: \\[ME\\] )?: Raises", sep = "")))){
                  self$players_in_hand[[j]]$preflop <- "Reraised open preflop"
                } else if(!is.na(str_match(preflop_action, paste(posit, " (?: \\[ME\\] )?: All\\-in\\(raise\\)", sep = "")))){
                  self$players_in_hand[[j]]$preflop <- "Reraised open preflop"
                } else if(!is.na(str_match(preflop_action, paste(posit, " (?: \\[ME\\] )?: Calls", sep = "")))){
                  self$players_in_hand[[j]]$preflop <- "Called open preflop"
                } else {
                  self$players_in_hand[[j]]$preflop <- "Folded after open"
                }
              }
            }
            
          }
        }
      } else {
        for(i in seq_len(self$num_players)){
          if(self$players_in_hand[[i]]$preflop != "Limped"){
            self$players_in_hand[[i]]$preflop <- "Folded before open"
          }
          if(self$players_in_hand[[i]]$position == "Big Blind"){
            if(length(limpers) != 0){
              if(!is.na(str_match(preflop_action, paste("Big Blind", " (?: \\[ME\\] )?: Raises", sep = "")))){
                self$BB$preflop <- "Squeezed limper in BB preflop"
              } else if(!is.na(str_match(preflop_action, paste("Big Blind", " (?: \\[ME\\] )?: All\\-in\\(raise\\)", sep = "")))){
                self$BB$preflop <- "Squeezed limper in BB preflop"
              } else {
                self$BB$preflop <- "Checked option"
              }
            } else {
              self$BB$preflop <- "Folded to BB"
            }
          }
        }
      }
      
     ## Determine amount of chips each player bet, add to pot
      action_string <- str_match(text, "([\\S\\s]+)Hand result")
      if(is.na(action_string[1,1])){
        action_string <- str_match(text, "([\\S\\s]+)Total Pot")
      }
      for(i in seq_len(self$num_players)){
        posit <- self$players_in_hand[[i]]$position
        if(posit == "UTG+1"){posit <- "UTG\\+1"}
        if(posit == "UTG+2"){posit <- "UTG\\+2"}
        
        ## Scan for bets
        s <- paste(posit, " (?: \\[ME\\] )?(?:: )?(?:Raises )?(?:Calls )?(?:Bets )?(?:All\\-in )?(?:All\\-in\\(raise\\) )?(?:Posts chip )?\\$([0-9\\.]{1,6})(?:[^\\[]){2}", sep = "")
        bets <- str_match_all(action_string, s)[[1]][,2]
        
        ## Adjustments
        if(posit == "Big Blind"){
          bets <- c(0.25, bets)
        }
        if(self$num_players == 2 & posit == "Dealer"){
          bets <- c(0.1, bets)
        }
        uncalled_bet <- str_match(text, paste(posit, " (?: \\[ME\\] )?: Return uncalled portion of bet \\$([0-9\\.]{1,6})", sep = ""))[2]
        if(!is.na(uncalled_bet)){
          bets <- c(bets, -1 * as.numeric(uncalled_bet))
        }
        
        #cat(posit, ":", bets, "\n")
        bets <- sum(as.numeric(bets))
        self$players_in_hand[[i]]$chips_bet <- bets
        self$Total_pot <- self$Total_pot + bets
        
        ## Determine if player was winner
        won <- str_match_all(text, paste(posit, " (?: \\[ME\\] )?: Hand result(?:\\-Side pot)? \\$([0-9\\.]{1,6})", sep = ""))[[1]][,2]
        if(length(won) > 1){
          won <- sum(as.numeric(won))
        }
        if(length(won) == 1){
          #cat(posit, ": ", won, "\n")
          self$players_in_hand[[i]]$chips_won <- as.numeric(won)
          self$Winnable_pot <- self$Winnable_pot + as.numeric(won)
          self$winners <- append(self$winners, self$players_in_hand[[i]])
        } 
        
        ## Finalize chips won
        self$players_in_hand[[i]]$chips_change <- self$players_in_hand[[i]]$chips_won - self$players_in_hand[[i]]$chips_bet
        
      }      
      
      ## Finalize pot, rake
      self$Rake <- self$Total_pot - self$Winnable_pot
      if(length(self$winners) == 0){
        self$winners = append(self$winners, self$BB)
      }
      
      
    }
  )
)



Player <- R6Class(
  classname = "Player",
  public = list(
    position = NA,
    chips = 0,
    hero = FALSE,
    card1 = NA,
    card2 = NA,
    hand = NA,
    chips_bet = 0,
    chips_won = 0,
    chips_change = 0,
    preflop = "No Action",
    initialize = function(position = NA, chips = 0, hero = FALSE){
      self$position = position
      self$chips = chips
      self$hero = hero
    },
    print = function(...){
      cat("Position:", self$position, "\n")
      cat("Chips:", self$chips, "\n")
      if(self$hero){
        cat("Player is the hero. \n")
      } else {
        cat("Player is a villain. \n")
      }
      cat("Hand: ", self$hand, " (", self$card1, self$card2, ") \n", sep = "")
      if(self$preflop == "Opened"){
        cat("Player opened the hand preflop. \n")
      } else if (self$preflop == "Folded before open"){
        cat("Player folded preflop before anyone opened. \n")
      } else if (self$preflop == "Folded after open"){
        cat("Player folded preflop after someone opened. \n")
      } else if (self$preflop == "Limped"){
        cat("Player limped preflop. \n")
      } else if (self$preflop == "Reraised open preflop"){
        cat("Player reraised an open preflop. \n")
      } else if (self$preflop == "Called open preflop"){
        cat("Player called an open preflop. \n")
      } else if (self$preflop == "Checked option"){
        cat("Player checked their option in BB preflop. \n")
      } else if (self$preflop == "Folded to BB"){
        cat("Player got a walk in the BB. \n")
      } else if (self$preflop == "Squeezed limper in BB preflop"){
        cat("Player opened over limpers in the BB. \n")
      }
      if(self$chips_change >= 0){
        cat("Player won $", self$chips_change, ". \n", sep = "")
      } else {
        cat("Player lost $", -1 * self$chips_change, ". \n", sep = "")
      }
    }
  )
)