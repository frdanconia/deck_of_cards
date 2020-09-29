require(R6)

# to moze nie jest najszybszy algorytm ale tak ludzie sortuja karty.
insertSort <- function(m, c){
    if(nrow(m) == 1){return(m)}
    for(i in 2:nrow(m)){
        key <- m[i,]
        j <- i - 1
        while(j >= 1 && m[j,c] > key[c]){
            m[j+1,] <- m[j,]
            j <- j - 1
        }
        m[j+1,] <- key
    }
    return(m)
}

card <- R6Class(classname = "Card",
                public = list(
                    name = NA,
                    color = NA,
                    initialize = function(name, color){
                        self$name <- name
                        self$color <- color
                    },
                    print = function(){
                        cat("Card ", self$name, "(", self$color, ")", sep = "")
                    },
                    getName = function(){
                        return(self$name)
                    },
                    getColor = function(){
                        return(self$color)
                    },
                    getCard = function(){
                        return(paste(self$name, self$color, sep = ""))
                    },
                    getValue = function(){
                        if(self$name == "A"){
                            return(c(1,11))
                        } else if(self$name %in% c("J","Q","K")){
                            return(10)
                        } else {
                            return(as.integer(self$name))
                        }
                    },
                    sortValName = function(){
                        if(self$name == "A"){return(14)} else
                        if(self$name == "K"){return(13)} else
                        if(self$name == "Q"){return(12)} else
                        if(self$name == "J"){return(11)} 
                        else {return(as.integer(self$name))}
                    },
                    sortValColor = function(){
                        if(self$color == "S"){return(4)} else
                        if(self$color == "H"){return(3)} else
                        if(self$color == "C"){return(2)} 
                        else(return(1))
                    }
                )
)

setOfCards <- R6Class(classname = "SetOfCards",
                      public = list(
                          hand = NA,
                          initialize = function(...){
                              self$hand <- list(...)
                          },
                          print = function(){
                              print(self$hand)
                          },
                          getSet = function(){
                              return(self$hand)
                          },
                          showSet = function(){
                              cat("Set of cards: ")
                              for(card in self$hand){
                                  cat(card$getCard(), " ")
                              }
                          },
                          shuffle = function(){
                              n <- length(self$hand)
                              ind <- sample(1:n, n)
                              tmp <- self$hand
                              for(i in 1:length(self$hand)){
                                  self$hand[i] <- tmp[ind[i]]
                              }
                              return(self)
                          },
                          score = function(){
                              val = 0
                              ace = 0
                              for(card in self$hand){
                                  cv <- card$getValue()
                                  if(length(cv) == 1){
                                      val <- val + cv[1]
                                  } else {
                                      ace <- ace + 1
                                  }
                              }
                              if(ace != 0){
                                  val <- val + 11*ace
                                  for(i in 1:ace){
                                      if(val > 21){
                                          val <- val - 10
                                      }
                                  }
                              }
                              if(val <= 21){
                                  return(val)
                              } else {
                                  return(-Inf)
                              }
                          },
                          addCards = function(...){
                              i <- length(self$hand) + 1
                              for(card in list(...)){
                                  self$hand[[i]] <- card
                                  i <- i + 1
                              }
                              return(self)
                          },
                          removeByIdx = function(ind){
                              self$hand[[ind]] <- NULL
                              return(self)
                          },
                          removeByName = function(name, color){
                              mark <- c()
                              for(i in 1:length(self$hand)){
                                  if(self$hand[[i]]$getName() == name && 
                                     self$hand[[i]]$getColor() == color){
                                      mark <- append(mark,i)
                                  }
                              }
                              self$hand[mark] <- NULL
                              return(self)
                          },
                          sample = function(n = 1){
                              if(n > length(self$hand)){
                                  return("Cannot draw more cards than in set")
                              }
                              cards <- self$hand[sample(1:length(self$hand),n)]
                              draw <- setOfCards$new()
                              for(i in 1:n){
                                  draw$addCards(cards[[i]])
                              }
                              return(draw)
                          },
                          sort = function(){
                              # sortuje od najmniejszej do najwiekszej karty
                              sMat <- data.frame() # for nrow to work with insertSort()
                              for(i in 1:length(self$hand)){
                                  sMat[i,1] <- self$hand[[i]]$sortValName()
                                  sMat[i,2] <- self$hand[[i]]$sortValColor()
                                  sMat[i,3] <- i # for tracking index of card
                              }
                              s1 <- insertSort(sMat,1)
                              for(i in unique(s1[,1])){
                                  s1[which(s1[,1]==i),] <- insertSort(s1[which(s1[,1]==i),], 2)
                              }
                              newHand <- list()
                              for(i in 1:nrow(s1)){
                                  newHand[[match(i,s1[,3])]] <- self$hand[[i]]
                              }
                              self$hand <- newHand
                              return(self)
                          }
                      )
)

deckOfCards <- R6Class(classname = "DeckOfCards",
                       inherit = setOfCards,
                       public = list(
                           hand = NA,
                           initialize = function(){
                               self$hand <- list()
                               for(color in c("S","H","C","D")){
                                   for(i in 2:10){
                                       super$addCards(card$new(as.character(i),color))
                                   }
                                   for(name in c("J","Q","K","A")){
                                       super$addCards(card$new(name,color))
                                   }
                               }
                           }
                       )
)

# przyklady
s1 <- setOfCards$new(card$new("A","S"), card$new("4","D"), card$new("Q","H"),
                     card$new("K","C"), card$new("2","S"), card$new("8","H"),
                     card$new("A","H"), card$new("A","C"), card$new("Q","D"),
                     card$new("J","D"), card$new("4","C"))
s1$shuffle()$showSet()
s1$sort()$showSet()

d1 <- deckOfCards$new()
d1$shuffle()$showSet()
d1$sort()$showSet()

d1$sample(3)$score()

pNgeS <- function(n, s, iter){
    # find prob that draw of n cards gives score greater or equal to s
    deck <- deckOfCards$new()
    scores <- c()
    for(i in 1:iter){
        scores <- append(scores, deck$sample(3)$score())
    }
    sum(scores >= s) / iter
}

pNgeS(3, 15, 1000)
# prawdopodobienstwo, ze losujac z talii 3 karty uzyskamy wynik nie mniejszy 
# niz 15 wynosi okolo 0.49