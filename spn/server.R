

server <- function(input, output, session) {
  
# options(warn=2)
  
source("server/observe.R",local = T)
source("server/reactive.R",local = T) 
source("server/Login.R",local = T)  
  
source("server/Fun/verbToTable.R",local = T) 
source("server/Fun/sent2function.R",local = T) 
  
#...Sentences  
source("server/Sents/SentLearn.R",local = T)
source("server/Sents/SentFlash6-4.R",local = T) 
source("server/Sents/SentQuiz.R",local = T)

#...Verbs
source("server/Verbs/VerbLearn.R",local = T)
source("server/Verbs/VerbFlash.R",local = T)  
source("server/Verbs/VerbQuiz.R",local = T)  
  
#...Vocab
source("server/Vocab/VocabLearn.R",local = T)
source("server/Vocab/VocabFlash.R",local = T)
source("server/Vocab/VocabQuiz.R",local = T)

  
source("server/home.R",local = T)    
source("server/testing.R",local = T)  
source("server/keyControl.R",local = T)  
  
  
#...Close app, end session capabilities  
  
exE_data <<- ""
exW_data <<- ""
exA_data <<- ""
global_user <<- ""
vls_cache <<- ""
vlr_cache <<- ""
  
session$onSessionEnded(function() {
  

  
  writeLines(paste(vls_cache,collapse = ","),
             con = paste0("userdata/",global_user,"/savedVerbs.txt"))
  
  writeLines(paste(vlr_cache,collapse = ","),
             con = paste0("userdata/",global_user,"/remVerbs.txt"))
  
  
  saveRDS(exDataG,paste0("userdata/Data.rds"))
  
  exDataG <<- NULL
  
  print(paste(tail(exDataG$row,n=5)))
  print(paste(tail(exDataG$type,n=5)))
  
  stopApp()
  
  
})


}











