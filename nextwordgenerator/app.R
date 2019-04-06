# "Next Word Generator"
# For any word or phrase of any length entered by the user, 
# the app will return up to three options for the next possible word

library(shiny)

# libraries for natural language processing
library(tm)
library(RWeka)
library(openNLP)

##############################################################################
# setting up core database and functions
##############################################################################
# run the script required to:
# download and read in original text data 
# create and clean a corpus from 1% of that data
# create, clean, and prune n-grams (2- to 5-grams) based on that corpus
# boost dictionary with external 2-gram from the  Corpus of Contemporary American English,  
# given our own computational resources being too limited to process more than 
# 1% of the original text data in reasonable time

source("prep.R") 

# load in processed database of n-grams: 2-gram to 5-gram
n2to5gram = read.csv("n2to5gram.csv", header = TRUE, stringsAsFactors = FALSE)

# loading common next words against parts-of-speech (POS) tags
unknown = read.csv("postags.csv", header = FALSE, stringsAsFactors = FALSE)

#****************************************************************
#****************************************************************

cleaninput <- function(x){
    
    # clean the input text to remove numeric
    # and punctuation data (except intraword apostrophe and hyphens), 
    # convert to lower case, and then split into words to search
    # if input text is longer than 4 words, retain only last 4 for search
    
    x = gsub("\\d+", "", x)
    x = gsub("\\s*(?:(?:\\B[-']+|[-']+\\B|[^-'[:^punct:]]+)\\s*)+", " ", x, perl = TRUE)
    x = tolower(x)
    x = strsplit(as.character(x), split = " ")[[1]]
    x = x[x != ""]
    
    ifelse(length(x) > 4, x <- tail(x,4), x <- x)
    
    x
    
}

#***************************************************************
#****************************************************************

searchxiny <- function(x, y){
    
    # search for input text in n-gram database
    # return "next word" options
    
    found = y[y[,1] == paste(x, collapse = " "), 2]
}

#****************************************************************
#****************************************************************

tagPOS <-  function(x, ...) {
    
    # need to explicitly add, despite inclusion of openNLP
    # required to tag part-of-speech
    
    s <- as.String(x)
    word_token_annotator <- Maxent_Word_Token_Annotator()
    a2 <- Annotation(1L, "sentence", 1L, nchar(s))
    a2 <- annotate(s, word_token_annotator, a2)
    a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
    
    gc() # release memory to avoid Out of Memory error
    
    a3[[length(a3)]]$features[[1]][[1]][1]
}

#***************************************************************
#***************************************************************

givenext <- function(x){
    
    # return upto three possible "next word" options
    # using a backoff model
    # unknowns handled with a combination of Penn Treebank tagging
    # and returning most common possible next words
    
    
    x = cleaninput(x)
    
    # exit if input has no words, only removable elements
    if(length(x)==0){return(found <- "Input should include at least one word")}
    
    startpoint = 1
    found = NA
    seek = NA
    
    # Backoff
    while(is.na(found[1]) & startpoint <= length(x)){
        seek = x[startpoint:length(x)]
        found = searchxiny(seek, n2to5gram)
        startpoint = startpoint + 1
    }
    
    # Unknown handling with POS tagging
    if(is.na(found[1])){
        pos =  tagPOS(seek)
        found <- searchxiny(pos, unknown)
    }else{
        found <- found[!is.na(found)]
    }
    
    found
    
}

#*******************************************************************

# UI to obtain user input and return corresponding output
# Will also offer usage instructions and other details

ui <- fluidPage(
    
    # Application title
    titlePanel(h1("Word! - the next word generator", style="color:#4682B4"), 
               windowTitle = "Word!"),
    
    fluidRow(br()),
  
      
      # Tabbed area accepting user input and offering app output
      # Also displays details of app functioning
      mainPanel(
          
         tabsetPanel(
             
             # First tab; accepts word/phrase input from user
             # Displays up to three next word options upon user's clicking Submit
             
             tabPanel(strong("Word!"), 
                      column(8, 
                             br(), 
                             p("Enter a word or phrase into the 
                               textbox below and click Submit when done.", 
                               style = "color:#4682B4; font-weight:bold"),
                             p(em("Feel free to include digits, special characters, 
                               and any number of words - even your own inventions!!")), 
                             br(),
                      textAreaInput("searchinput", label = "Enter text here:"),
                      actionButton("find", "Submit", 
                                   style = "background-color:#4682B4;
                                   font-weight:bold; color:#FFFFFF"),
                      br(),hr(),
                      p("For continuous typing experience, try", 
                        strong("Insta-Word!", style = "color:#4682B4"),
                        "in the next tab - no need to Submit")
                      ),
                        
                      
                      column(4, 
                      sidebarPanel(style = "background-color:#F5F5F5; height:350px",
                                   p("Your next word option(s)", align = "center",
                                     style = "font-size:1.2em"),
                                   br(), br(), br(),
                                   htmlOutput("found"), 
                                   width = "100%" 
                                  )
                      )
                      
                      
                      
                      ),

             
             # Second tab; accepts word/phrase input from user
             # Continuously displays up to three next word options against the typing
             
             tabPanel(strong("Insta-Word!"),
                      column(8, 
                             br(), 
                             p("Just keep typing and see the next word options change 
                               alongside ...", em("may need to type a little slowly"), 
                               style = "color:#4682B4; font-weight:bold"),
                             p(em("Feel free to include digits, special characters, and 
                                  any number of words - even your own inventions!!")),
                             br(),
                             
                             textAreaInput("instasearchinput", label = "Enter text here:"),
                             actionButton("instafind","", style="border-color:#FFFFFF")
                             ),
                                           
                                           
                      column(4, 
                             sidebarPanel(style = "background-color:#F5F5F5; height:350px",
                                          p("Your next word option(s)", align = "center",
                                            style = "font-size:1.2em"),
                                          br(), br(), br(),
                                          htmlOutput("instafound"), 
                                          width = "100%" 
                             )
                             )
                      ),
             
             # Third tab; contains details about the app's functioning
             tabPanel(strong("About"),
                      br(),
                      p("Developed by Navya Kumar and uploaded on -April-2019."),
                      br(),
                      p("To know more about the functioning of this app, please visit:"),
                      a("https://sites.google.com/site/nannkriti/", 
                        href="https://sites.google.com/site/nannkriti/"),
                      br(), br(),
                      p("For the code for this app, please visit:"),
                      a("https://sites.google.com/site/nannkriti/", 
                        href="https://sites.google.com/site/nannkriti/")
                      )
             
             
         ) 
          
      )
   )


# Server logic to return up to three next word options based on user input
server <- function(input, output) {
    
    # wait on clicking of Submit to trigger searching of input phrase
    result <- eventReactive(input$find, {givenext(input$searchinput)})
    
    # return formatted search results
    output$found <- renderUI({
        
        found = result()
        found[which(found == "i")] = "I"
        p(paste(head(found,3), collapse = "  /  "),
          style = "font-size:1.6em; font-weight:bold", 
          align = "center")

    })
    
    # continuously search and return formatted results
    output$instafound <- renderUI({
        
        instafound <- givenext(input$instasearchinput)
        instafound[which(instafound == "i")] = "I"
        p(paste(head(instafound,3), collapse = "  /  "),
          style = "font-size:1.6em; font-weight:bold", 
          align = "center")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

