
# --------------------------------
# tab selection when click on image
# --------------------------------

shinyjs::onclick("fisheryID",  updateTabsetPanel(session, inputId="navbar", selected="Fishery overview"))
shinyjs::onclick("sampleID",  updateTabsetPanel(session, inputId="navbar", selected="Sampling overview"))
shinyjs::onclick("stockID",  updateTabsetPanel(session, inputId="navbar", selected="Biology overview"))



# --------------------------------
# modal disclaimer
# --------------------------------

# the modal dialog where the user agree in terms
disclaimer_modal <- modalDialog(
  title = "Terms of use",
  includeHTML("data/DescriptionDisclaimer.txt"),
  easyClose = T, #set to F if you want people to click the "I agree" button
  footer = tagList( downloadButton("report", "Download Disclaimer"),
                    actionButton("ok", "I agree")
  )     
)

# Show the modal on start up ...
showModal(disclaimer_modal)

# ... close when agree
observeEvent(input$ok,{
  removeModal()
})



# --------------------------------
# modal boxes (on startpage)
# --------------------------------
observeEvent(input$modal1,{
  showModal(modalDialog(
    tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                src="DCF Introduction.pdf"),
    title= "Introduction to German DCF program",
    easyClose = TRUE,
    footer = NULL))
})
observeEvent(input$modal2,{
  showModal(modalDialog(
    tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                src="DCF Objectives.pdf"),
    title= "Project Objectives",
    easyClose = TRUE,
    footer = NULL))
})
observeEvent(input$modal3,{
  showModal(modalDialog(
    tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                src="Sampling Design.pdf"),
    title= "Sampling Design",
    easyClose = TRUE,
    footer = NULL))
})
observeEvent(input$modal4,{
  showModal(modalDialog(
    tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                src="Sampling Methodology.pdf"),
    title= "Methodology",
    easyClose = TRUE,
    footer = NULL))
})
output$moreInfo <- renderText("Click the buttons below for more
            information on the Thünen-OF Sampling program")


