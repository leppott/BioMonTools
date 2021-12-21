# About Panel

function() {
  tabPanel("tabpan_about"
           , fluidPage(h2("About"
                          #, style  = "text-align:center"
                          )
                       , p("Background info")


                        )## fluidPage ~ END
           # , includeHTML(file.path("external", "Help.html"))
           )##tabPanel ~ END
}##FUNCTION ~ END

# output$UI_about = renderUI({
#   p("About stuff here.")
# })
