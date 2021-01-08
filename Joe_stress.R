#Script to read and plot stressor values

#reading in an excel file
dose<<-read_xlsx(file.name.2,sheet=scn.run[1])
dose


#Plot
#Select the stressors and HUCs to plot
# stressor.select<-unique(dose$Stressor)
# huc.select<-unique(dose$HUC_ID)
# dose.huc<-dose[dose$HUC_ID%in%huc.select&dose$Stressor%in%stressor.select,]
# dose.huc
# dose.plot(dose.huc)
