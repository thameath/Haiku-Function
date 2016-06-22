haiku <- function(debug = F){
  
  linelengths <- c(5,7,5)
  
  #load files#
  wordlist <- read.csv("//chse.ohusu"
                       "H:/Haiku/wordlist.csv")
  
  for(i in 1:length(linelengths)){
    
    remain <- linelengths[i]
    
    linenum <- paste0("line",i)
    
    noun_L <- sample(1:4,
                     size = 1,
                     prob = 2*(4:1))
    
    remain <- remain - noun_L
    range <- ifelse(remain < 4, remain, 4)
    
    verb_L <- sample(1:range,
                     size = 1,
                     prob = 2*(range:1))
    
    remain <- remain - verb_L
    range <- ifelse(remain < 4, remain, 4)
    
    adv_L <- 0
    
    startpoint <- ifelse(range > 4, range - 4, 0)
    
    if(range > 0){
      adv_L <- sample(startpoint:range,
                      size = 1,
                      prob = 2*((range+1):(startpoint + 1)))
    }
    
    remain <- remain - adv_L
    adj_L <- remain
    
    noun <- sample(wordlist[wordlist$Part.of.speech == "n" &
                              wordlist$snum == noun_L,]$Word,1)
    verb <- paste0(" ", sample(wordlist[wordlist$Part.of.speech == "v" &
                                          wordlist$snum == verb_L,]$Word,1))
    adj <- ifelse(adj_L > 0,
                  paste0(sample(wordlist[wordlist$Part.of.speech == "j" &
                                           wordlist$snum == adj_L,]$Word, 1), " "),
                  "")
    adv <- ifelse(adv_L > 0,
                  paste0(" ", sample(wordlist[wordlist$Part.of.speech == "r" &
                                                wordlist$snum == adv_L,]$Word, 1)),
                  "")
    
    if(!grepl("s$",noun) | grepl("ss$",noun)){
      if(!grepl("s$", verb)){
        verb <- gsub("$","s",verb)
      }
      
      #if(grepl("ss$", verb)){
      #  verb <- gsub("ss$","sses",verb)
      #}
    }
    
    if(grepl("s$",noun) & !grepl("ss$",noun)){
      
      #if(grepl("sses$", verb)){
      #  verb <- gsub("es$","",verb)
      #}
      
      if(grepl("s$", verb) | !grepl("ss$",verb)){
        verb <- gsub("s$","",verb)
      }
      
    }
    
    assign(linenum, paste0(adj, noun, verb, adv))
    
    if(debug == T){message(sprintf("Line %s: %s\nAdj: %s\nNoun: %s\nVerb: %s\nAdverb: %s\n\n%s", 
                                   i,adj_L + noun_L + verb_L + adv_L,
                                   adj_L, noun_L, verb_L, adv_L, get(linenum)))}
    
  }
  
  message(paste(line1, line2, line3, sep="\n"))
  
}