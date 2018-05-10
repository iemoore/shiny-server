

server <- function(input, output, session) {
  
options(warn=2)
  
source("server/observe.R",local = T)
source("server/reactive.R",local = T) 
source("server/Login.R",local = T)  
  
#...Sentences  
source("server/Sents/SentLearn.R",local = T)
source("server/Sents/SentFlash.R",local = T) 
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
exW_data <<- ""
global_user <<- ""
vls_cache <<- ""
vlr_cache <<- ""
  
session$onSessionEnded(function() {
  
  stopApp()
  
  writeLines(paste(vls_cache,collapse = ","),
             con = paste0("userData/",USER$name,"/savedVerbs.txt"))
  
  writeLines(paste(vlr_cache,collapse = ","),
             con = paste0("userData/",USER$name,"/remVerbs.txt"))
  
  writeLines(paste(exE_data,collapse = ","),
             paste0("userData/",global_user,"/excludeE.txt"))
  
  writeLines(paste(exW_data,collapse = ","),
             paste0("userData/",global_user,"/excludeW.txt"))
  
  writeLines(paste(exA_data,collapse = ","),
             paste0("userData/",global_user,"/excludeA.txt"))
  
  
})


}











