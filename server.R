
# Define server logic 
function(input, output, session) {
  
  observeEvent(input$ce.button,{
    MC.sims<<-isolate(input$MC.sims)  #update the number of MC simulations
    scn.run<<-strsplit(isolate(input$scn.names),",")[[1]]
    num.scn<-length(scn.run); scn.run.num<<-1
    withProgress(message="Run...Joe...run",value=0.5,{
      scn.list.out<<-list() #create a new list to store output from each scenario
      for (i in 1:num.scn){
        #run model
        source("Joe_model.R")
        scn.list.out[[scn.run[i]]]<<-list(ce.df=ce.df,sc.dose.df=sc.dose.df)
        scn.run.num<<-scn.run.num+1
      }
    })
    rv.model$update<-rv.model$update+1 #trigger reactive variable for model
    rv.map$update<-rv.map$update+1  #trigger reactive variable for map
    rv.stress$update<-rv.stress$update+1 #activate re-plot of stressor dose
    #slider.dose<<-FALSE #set slider back to FALSE after a model run
  })#end observeEvent
  
  #mymap server function
  output$mymap <- renderLeaflet({
    #redraw map when reactive variable rv.map triggered 
    #no need to re-run the whole model if a HUC is merely
    #being highlighted so separate reactive values
    rv.map$update
    
    #set HUC colours
    ce.mn<-ddply(ce.df,~HUC,mn.sd.huc)
    #join cumulative effects data.frame to shapefile
    #join() preserves the order of the first
    #you need to change the "HUC" column to the appropriate name
    #uses the unquote !! of tidyeval with :=  (thanks Google)
    ce.mn<-dplyr::rename(ce.mn,!!HUC_Name:=HUC)
    HUC.Map@data<-join(HUC.Map@data,ce.mn,by=HUC_Name)
    #set FSI colours #for HTML colors: https://htmlcolorcodes.com/
    HUC.Map@data$COLOR[HUC.Map@data$mean>=0.98]<-"#149813"  #give 5 to HUCs that are darn close #Dark green
    HUC.Map@data$COLOR[HUC.Map@data$mean<0.98]<-"#01FC0F"  #"light green" is producing black? #Light green
    HUC.Map@data$COLOR[HUC.Map@data$mean<=0.7]<-"#F4FB0E"#yellow
    HUC.Map@data$COLOR[HUC.Map@data$mean<=0.5]<-"#FEB100" #orange
    HUC.Map@data$COLOR[HUC.Map@data$mean<=0.2]<-"#FF0000" #red
    HUC.Map@data$COLOR[HUC.Map@data$mean<=0]<-"#000000"#black
    HUC.Map@data$COLOR[is.na(HUC.Map@data$mean)]<-"#797878" #grey
    #set FSI colour alphas based on 3*sd relative to 0.2
    HUC.Map@data$alpha<-1-(3*HUC.Map@data$sd)/0.2
    HUC.Map@data$alpha[HUC.Map@data$alpha<0.1]<-0.2  #keep minimum alpha at 0.2 for visibility
    HUC.Map@data$alpha[is.na(HUC.Map@data$alpha)]<-0.2 #set unknown HUCs to a partly shaded grey
    head(HUC.Map@data)
    
    #bold the selected HUC (input$sel.huc is reactive)
    HUC.Map@data$border<-NA
    huc.map<-input$sel.huc
    huc.exist<-huc.map %in% HUC.Map@data[,colnames(HUC.Map@data)==HUC_Name]
    if (huc.exist){
      #change border width for the selected HUC
      HUC.Map@data$border[grep(huc.map,
                               HUC.Map@data[,colnames(HUC.Map@data)==HUC_Name])]<-10
    }
    #and create the map using leaflet and piped options
    m.base.map <- leaflet() %>%
      addTiles() %>% # Add default OpenStreetMap map tiles
      addProviderTiles("Stamen.Terrain") %>%
      setView(-114, 54, zoom = 5)
      #flyTo(-114, 54, zoom = 5)
   
    #popup content
    #change to be flexible with variable HUC_Name
    #browser()
    popup_info <- paste("HUC:",eval(parse(text=paste0("HUC.Map$",HUC_Name))),"\n","Name:",
                        HUC.Map$NAME,"\n","Score:",round(HUC.Map@data$mean,3),sep = '<br/>')
    # popup_info <- paste("HUC:",HUC.Map$HUC_10,"\n","Name:",
    #                     HUC.Map$NAME,"\n","Score:",round(HUC.Map@data$mean,3),sep = '<br/>')
    
    #add polygons
    m.base.map %>%
      addPolygons(data=HUC.Map,weight=2,col ='black',
                  fill=T,fillColor = HUC.Map@data$COLOR,
                  fillOpacity = HUC.Map@data$alpha,
                  popup = popup_info
                  ) %>%
      addPolygons(data=HUC.Map,weight=HUC.Map@data$border,col='white',
                  fill=F) 
  }) #end mymap server function
 
  #CE.plot function
  output$CE.plot<-renderPlot({
    rv.model$update #reactive variable to update figure when model is re-run
    cum.eff.plot(ce.df)
  })
  
  #System capacity by stressor for a given HUC plot
  output$SC.HUC.plot<-renderPlot({
    rv.model$update #reactive variable to update figure when model is re-run
    sel.huc<-input$sel.huc  #input$sel.huc is reactive
    #only renderplot if HUC exists
    huc.exist<-sel.huc %in% row.names(sys.capacity)
    if (huc.exist){
      huc.df<-cbind(simulation=1:length(sys.capacity[1,1,]),data.frame(t(sys.capacity[sel.huc,,])))
      huc.df
      huc.df<-pivot_longer(huc.df,cols=stressors,
                           names_to="stressor",values_to="sys.cap")
      head(huc.df)
      huc.plot(huc.df,sel.huc)
    }
    
  })
  
  #Stressor plot for HUC
  output$Str.HUC.plot<-renderPlot({
    sel.huc.str<<-input$sel.huc.str #this is reactive and set global
    sel.str<<-input$sel.str #input$sel.str is reactive and set global
    rv.stress$update #reactive value triggered when a new dose file read 
    dose.huc<-dose[dose$HUC_ID%in%sel.huc.str&(dose$Stressor%in%sel.str|
                                                 dose$Sub_Type%in%sel.str),]
    #only renderplot if something exists in the tibble
    str.exist<-!is.na(dose.huc[1,1])
    if (str.exist){
      dose.plot(dose.huc)
    }
  })
  
  #Sliders for stressor dose
  #only render if 1 HUC, 1 Stressor and 1 scenario selected
  #first mean slider
  output$stress.slider<-renderUI({
    rv.model$update #reactive variable when model has been re-run
    #only show slider when 1 huc, 1 stressor and 1 scenario! These are all reactive
    if (length(input$sel.huc.str)==1&length(input$sel.str)==1&
        length(strsplit(input$scn.names,",")[[1]])==1){
      #rv.stress$update<-rv.stress$update+1 #activate re-plot and dose table update
      #work with Sub_Type names (Sub_Type = main if not additive)
      sliderInput("slider.dose.mn","Set stressor dose",
                  min=dose$Low_Limit[dose$HUC_ID==sel.huc.str&dose$Sub_Type==sel.str][1],
                  max=dose$Up_Limit[dose$HUC_ID==sel.huc.str&dose$Sub_Type==sel.str][1],
                  value=dose$Mean[dose$HUC_ID==sel.huc.str&dose$Sub_Type==sel.str][1])
    }else {
       h4("Sliders only appears if one HUC, one stressor and one scenario are selected.")
    }
  })
  
  #second slider for SD (this might be better as numericInput??)
  output$stress.slider.sd<-renderUI({
    rv.model$update #reactive variable when model has been re-run
    #only show slider when 1 huc, 1 stressor and 1 scenario! These are all reactive
    if (length(input$sel.huc.str)==1&length(input$sel.str)==1&
        length(strsplit(input$scn.names,",")[[1]])==1){
      #rv.stress$update<-rv.stress$update+1 #activate re-plot and dose table update
      #work with Sub_Type names (Sub_Type = main if not additive)
      #set maximum SD to 2X mean dose level or 1 whichever is greater
      max.sd.slide<-max(2*input$slider.dose.mn,1,na.rm=T)
      sliderInput("slider.dose.sd","Set stressor SD",
                  min=0,
                  max=max.sd.slide,
                  value=dose$SD[dose$HUC_ID==sel.huc.str&dose$Sub_Type==sel.str][1],
                  step = 0.01)
    }
  })
  
  #update dose table when mean slider is activated or changed
  #also remove the scenario name from CE tab
  observeEvent(input$slider.dose.mn,{
    #update dose table if slider has been activated  
    dose$Mean[dose$HUC_ID==sel.huc.str&          
                  dose$Sub_Type==sel.str]<<-input$slider.dose.mn 
    rv.stress$update<-rv.stress$update+1 #activate re-plot of stressor dose
    read.dose<<-FALSE #don't read in doses from Excel if using slider input
    #remove scenario name in CE tab to 
    updateTextInput(session,"scn.names",value="<USER DEFINED>")
  })
  
  #update dose table when SD slider is activated or changed
  #also remove the scenario name from CE tab
  observeEvent(input$slider.dose.sd,{
    #update dose table if slider has been activated  
    dose$SD[dose$HUC_ID==sel.huc.str&          
                dose$Sub_Type==sel.str]<<-input$slider.dose.sd 
    rv.stress$update<-rv.stress$update+1 #activate re-plot of stressor dose
    read.dose<<-FALSE #don't read in doses from Excel if using slider input
    #remove scenario name in CE tab to 
    updateTextInput(session,"scn.names",value="<USER DEFINED>")
  })
  
  # #trigger an update to read dose table from Excel file when new scenarios
  observeEvent(input$scn.names,{
    read.dose<<-TRUE
  })
  
  #Stress-response function to plot
  output$Str.resp.plot<-renderPlot({
    sel.str.resp<-isolate(input$sel.str.resp) #this is not reactive
    str.resp.sim<-isolate(input$str.resp.sim) #this is not reactive
    input$run.str.resp #this IS reactive (button pushed) to update plot
    #only renderplot if something exists
    str.resp.exist<-!is.null(sel.str.resp)
    if (str.resp.exist){
      sel.pnt<-grep(sel.str.resp,stressors)
      log.tran<-if (main.sheet[sel.pnt,4]=="linear") FALSE else TRUE
      if (log.tran){
        min.stress<-min(log((stressor.list[[sel.pnt]][,1])))
        max.stress<-max(log((stressor.list[[sel.pnt]][,1])))
      }else{
        min.stress<-min((stressor.list[[sel.pnt]][,1]))
        max.stress<-max((stressor.list[[sel.pnt]][,1]))
      }
      stres.resp.plot(mean.resp.list[[sel.pnt]],min=min.stress,
                      max=max.stress,log.boo=log.tran,sims=str.resp.sim,stress.name=sel.str.resp)
    }
  })
  
  #read in new Stressor doses from file
  observeEvent(input$dose.in,{
    #read in new doses
    dose.Excel<-input$dose.in 
    file.name.2<<-dose.Excel$datapath
    #De-activate slider value, set selected HUC to NULL and trigger new stressor file uploaded
    stress.slider<<-FALSE; sel.huc.str<-NULL
    #set trigger to read in new dose file
    read.dose<<-TRUE
    #trigger reactive value to re-plot stressor doses
    rv.stress$update<-rv.stress$update+1
  })
  
  #read in new stressor-response relations
  observeEvent(input$str.resp.in,{
    #read in new stressor-response relations
    str.resp.Excel<-input$str.resp.in 
    file.name<<-str.resp.Excel$datapath
    source("Joe_curve.R")
  })
  
  #Download the individual stressor data in csv format
  output$dose.str.data.out<-downloadHandler(
    filename=function(){
      paste0(isolate(input$file.prefix.dose.str),".csv") #isolate filename so not reactive
    },
    content=function(file){
      out.sc.dose.df<-NULL
      #create data.frame entries for each scenario run
      #probably a quicker way using nested ldply than loop?
      for (i in 1:length(scn.run)){  
        if(is.null(out.sc.dose.df)){
          out.sc.dose.df<-data.frame(scenario=rep(scn.run[i],
                                             length(scn.list.out[[i]]$sc.dose.df$HUC)),
                                scn.list.out[[i]]$sc.dose.df)
        }else {
          out.sc.dose.df<-rbind(out.sc.dose.df,
                           data.frame(scenario=rep(scn.run[i],
                                                   length(scn.list.out[[i]]$sc.dose.df$HUC)),
                                      scn.list.out[[i]]$sc.dose.df))
        }
      }
      write.csv(out.sc.dose.df,file)
    }
  )
  
  #Download the CE data in csv format
  output$ce.data.out<-downloadHandler(
    filename=function(){
      paste0(isolate(input$file.prefix.ce),".csv") #isolate filename so not reactive
    },
    content=function(file){
      out.ce.df<-NULL
      #create data.frame entries for each scenario run
      #probably a quicker way using nested ldply than loop?
      for (i in 1:length(scn.run)){  
        if(is.null(out.ce.df)){
          out.ce.df<-data.frame(scenario=rep(scn.run[i],
                                             length(scn.list.out[[i]]$ce.df$HUC)),
                                scn.list.out[[i]]$ce.df)
        }else {
          out.ce.df<-rbind(out.ce.df,
                           data.frame(scenario=rep(scn.run[i],
                                                   length(scn.list.out[[i]]$ce.df$HUC)),
                                      scn.list.out[[i]]$ce.df))
        }
      }
      write.csv(out.ce.df,file)
    }
  )
  
} #end server function