# About Panel

function() {
  tabPanel("tabpan_about"
           , h2("About")
           , p(paste0("Version: ", pkg_version))
           # , fluidPage(h2("About"
           #                #, style  = "text-align:center"
           #                )
           #             , p("Background info")
           #
           #             , p("Version 0.5.0.9080")
           #
           #
           #              )## fluidPage ~ END
           # html_fragment
           , includeHTML(file.path("www", "ShinyHTML_About.html"))
         #  , htmlOutput("html_about") # use with iframe


          , tableOutput("tbl_about")

          , em("Last updated: 2022-01-27")


           )##tabPanel ~ END
}##FUNCTION ~ END
