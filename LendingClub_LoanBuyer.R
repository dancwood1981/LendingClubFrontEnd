display.mode="showcase"

ui <- fluidPage(
          headerPanel("Lending Club")        
        , selectInput(
            inputId = "SelectedGrade",
            label = "Select grade",
            choices = c("A", "B", "C", "D", "E", "F", "G"))
        , uiOutput("LoansCheckbox")
        , tableOutput(outputId = "PrimaryTable")
        , tableOutput(outputId = "SecondaryTable")
        , actionButton("BuyButton", "Buy Selected Loans"))
    

server <- function(input, output) {
  # Bring in libraries
  library(shiny)
  library(LendingClub)
  
  # User to select Grade of Loan
  output$grade <- renderText({print(input$SelectedGrade)})

  # setup access to my account
  DanWood_cred <- lc_MakeCredential("604420","d6TON1sIdNV5onCdPjDxI3zhjyY=")
  lc_AccountSummary(LC_CRED = DanWood_cred)
  LoanVars <- c("id", "intRate", "grade")
  
  # Download avaiable loans
  PrimaryNotes <- lc_ListedLoans(showAll = TRUE, LC_CRED = DanWood_cred)
  PrimaryNotes_loans <- reactive({as.data.frame(PrimaryNotes$content$loans)})
  
  # Get Scoring Code from Dropbox
  #library(rdrop2)
  #modelDir <- "LendingClubPrimary"
  
 
  
  loadData <- function() {
    # Read all the files into a list
    filesInfo <- drop_dir(outputDir)
    filePaths <- filesInfo$path
    data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
  }
  
  # SecondaryNotes <- FolioListing()
  # SecondaryNotes <- FolioListing(updatedSince = NULL, LC_CRED = DanWood_cred)
  
  # Create list of loans to buy
  LoansToBuy_Primary <- reactive({subset(PrimaryNotes_loans(),id %in% input$LoansCheckbox, select = id )}) 

  # Load subsetted loans into checkboxes
  output$LoansCheckbox <- renderUI({
                            choice <-  PrimaryNotesSelectedGrade()
                            checkboxGroupInput(
                                "LoansCheckbox"
                              , "Select Loans to Buy"
                              , choices = PrimaryNotesSelectedGrade()[1:5,"id"]
                              , selected = PrimaryNotesSelectedGrade())})
      
  # Buy selected Loans after button is pushed
  observeEvent(
      input$BuyButton
    , {lc_SubmitOrder( 
          as.integer(as.data.frame(t(LoansToBuy_Primary()))) 
        , amount = 25
        , portfolioId = NULL 
        , LC_CRED = DanWood_cred
        , quiet = T)}) 
    
  # Load subsetted loans into table
  output$LoanTable <- renderUI({
                        choice <-  PrimaryNotesSelectedGrade()
                        checkboxGroupInput(  "LoansCheckbox"
                                           , "Select Loans to Buy"
                                           , choices = PrimaryNotesSelectedGrade()[,"id"]
                                           , selected = choice[1])})
      
  # Show all loans available for each 'grade' in a table  
  PrimaryNotesSelectedGrade <- reactive ({subset(  PrimaryNotes_loans(), grade %in% input$SelectedGrade)})
  
  # Show all Primary Loans that are checked and ready to purchase
  output$PrimaryTable <- renderTable({subset(x = PrimaryNotes_loans(),
                                             subset = id %in% input$LoansCheckbox 
                                             ,select = c(term, intRate)
                                             )
                                    })
  
  # Show the contents of LoansToBuy_Primary
  output$LTB <- renderTable({LoansToBuy_Primary()})
  
  # Display Secondary notes in table
  # output$SecondaryTable <-  renderTable(head(SecondaryNotes))

  
  
    
  
}
shinyApp(ui = ui, server = server)


  


