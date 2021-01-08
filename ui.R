

###########################
#Functions

###########################


# Define UI for Weekly Fish Model
fluidPage(
  # Application title
  titlePanel("XXXX Cumulative Effects Model"),
  #Use navbarPage layout
  navbarPage("",
    
    tabPanel("Map",
      fluidRow(
        leafletOutput("mymap", width="100%",height=600),
        h3("")   
      )#end fluidRow            
    ),#end tabPanel Maps
    
    tabPanel("CE",
      plotOutput("CE.plot"),
      numericInput("MC.sims","Number of simulations",MC.sims,min=1,max=10000,step=100),
      textInput("scn.names","Name of scenarios to run",scn.run),
      actionButton("ce.button","Run Joe model")
    ),#end tabPanel CE
    
    tabPanel("HUC",
      column(width=4,
             wellPanel(selectInput("sel.huc","HUC to plot",choices=c("",unique(dose$HUC_ID))))
             ),
      plotOutput("SC.HUC.plot")
    ),#end tabPanel HUC
    
    tabPanel("Stressor Levels",
      column(width=4,
                    wellPanel(selectInput("sel.huc.str","HUCs to plot",
                                                 choices=unique(dose$HUC_ID),
                                          multiple=TRUE))
             ),
      column(width=4,
             wellPanel(selectInput("sel.str","Stressors to plot",
                                          choices=unique(dose$Sub_Type),
                                   multiple=TRUE))
      ),
      column(width=4,
             wellPanel(
               uiOutput("stress.slider"),
               uiOutput("stress.slider.sd")
             )
      ),
      plotOutput("Str.HUC.plot")
    ),#end tabPanel 
    
    tabPanel("Stress-Response Relations",
             column(width=4,
                    wellPanel(radioButtons("sel.str.resp","Stress-response function to plot",
                                                 choices=unique(main.sheet$Stressors),selected=""))
                    ),
             column(width=2,wellPanel(numericInput("str.resp.sim","Number of simulations for curve",
                                                   value=10,min=1,max=1000,step=10))),
             column(width=2,wellPanel(actionButton("run.str.resp","Plot relation"))),
             plotOutput("Str.resp.plot")
    ),#end tabPanel
    
    tabPanel("Data Input/Output",
      wellPanel(
        fileInput("dose.in","Input stressor doses.",
                  accept=".xlsx"),
        h5("New stressor doses do not effect cumulative effects until model is re-run!")
      ),
      wellPanel(
        fileInput("str.resp.in","Input stressor-response relations.",
                  accept=".xlsx"),
        h5("New stressor-response relations do not effect cumulative effects until model is re-run!")
      ),
      wellPanel(
        textInput("file.prefix.dose.str",label="Prefix for individual stressor output",
                  value="stressor_output"),
        downloadButton('dose.str.data.out','Download individual stressor data (.csv)')    
      ),
      wellPanel(
        textInput("file.prefix.ce",label="Prefix for overall cumulative effects output",
                                    value="ce_output"),
        downloadButton('ce.data.out','Download cumulative effects data (.csv)')    
      )
      #   
    )  #end tabPanel Data Input/Output
  )#end navbarPage
)#end Fluidpage
      
   
