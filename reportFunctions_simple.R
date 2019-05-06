# Dummy min/maxDate object so you can build these objects outside the function
minDate <- as.Date('2000-05-01')
maxDate <- as.Date('2020-05-01')

# Build specifics for each plot by parameter
DOsettingsThresholds <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=10, ymax=Inf, alpha=1, fill="#0072B2") ,
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=8, ymax=10, alpha=1, fill="#009E73") ,
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=7, ymax=8, alpha=1, fill="#F0E442") ,
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=7, alpha=1, fill="firebrick" ) ,
  geom_hline(yintercept = 4, color="black", size=1.5),
  scale_y_continuous(name="Dissolved Oxygen mg/L", breaks=seq(4,12,2), limits=c(4,12)) ,
  labs(x="Sample Month-Year", y="Dissolved Oxygen mg/L"))

pHsettings <- list(
  geom_hline(yintercept = 6, color="black", size=1.5),
  geom_hline(yintercept = 9, color="black", size=1.5),
  scale_y_continuous(name="pH", breaks=seq(5,10,.5), limits=c(5,10)),
  labs(x="Sample Month-Year", y="pH"))

spCondsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=250, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=250, ymax=350, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=350, ymax=500, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=500, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Specific Conductivity", breaks=seq(30, 550,50),limits=c(30,550)),
  labs(x="Sample Month-Year", y="Specific Conductivity"))

TDSsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=100, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=100, ymax=250, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=250, ymax=350, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=350, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Total Dissolved Solids mg/L", breaks=seq(50, 400,50), limits=c(50,400)),
  labs(x="Sample Month-Year", y="Total Dissolved Solids"))

Sulfatesettings <- list(
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=10, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=10, ymax=25, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=25, ymax=75, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=75, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Dissolved Sulfate mg/L", breaks=seq(0, 80,10), limits=c(0,80)),
  labs(x="Sample Month-Year", y="Dissolved Sulfate mg/L"))

Chloridesettings <- list(
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=10, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=10, ymax=25, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=25, ymax=50, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=50, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Dissolved Chloride mg/L", breaks=seq(0, 55,5), limits=c(0,55)),
  labs(x="Sample Month-Year", y="Dissolved Chloride mg/L"))

Potassiumsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=1, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=1, ymax=2, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=2, ymax=10, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=10, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Dissolved Potassium mg/L", breaks=seq(0, 12,2),limits=c(0,12)),
  labs(x="Sample Month-Year", y="Dissolved Potassium mg/L"))

Sodiumsettings <- list(
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=7, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=7, ymax=10, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=10, ymax=20, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=20, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Dissolved Sodium mg/L", breaks=seq(0, 30,5), limits=c(0,30)),
  labs(x="Sample Month-Year", y="Dissolved Sodium mg/L"))

TNsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=.5, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=.5, ymax=1, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=1, ymax=2, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=2, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Total Nitrogen mg/L", breaks=seq(0, 3,.5), limits=c(0,3)),
  labs(x="Sample Month-Year", y="Total Nitrogen mg/L"))

TPsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=.02, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=.02, ymax=.05, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=.05, ymax=.1, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=.1, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Total Phosphorus mg/L", breaks=seq(0, .2,.05),limits=c(0,0.2)),
  labs(x="Sample Month-Year", y="Total Phosphorus mg/L"))

TotHabsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=150, ymax=Inf, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=130, ymax=150, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=100, ymax=130, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=100, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Total Habitat", breaks=seq(0, 200, 25),limits=c(0,200)),
  labs(x="Sample Month-Year", y="Total Habitat"))

LRBSsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=-1.5, alpha=1, fill="firebrick"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-1.5, ymax=-1, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-1, ymax=-0.5, alpha=1, fill="#009E73"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-0.5, ymax=0.5, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=0.5, ymax=Inf, alpha=1, fill="#F0E442"),
  scale_y_continuous(name="Log Relative Bed Stability", breaks=seq(-2, 1,.5),limits=c(-2, 1)),
  labs(x="Sample Month-Year", y="Log Relative Bed Stability"))

Tempsettings <- list(
  geom_hline(yintercept =32, color="black", size=1),
  scale_y_continuous(name="Temperature C", breaks=seq(0,35, 5), limits=c(0,35)),
  labs(x="Sample Month-Year", y="Temperature"))

MetalsCCUsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=0.75, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=0.75, ymax=1.5, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=1.5, ymax=2.0, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=2.0, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Dissolved Potassium mg/L", breaks=seq(0, 2.5,0.25),limits=c(0,2.5)),
  labs(x="Sample Month-Year", y="Dissolved Potassium mg/L"))




gMasterPlot <- function(dataset,parameter, datebreaks){
  dat <- dplyr::select(dataset,StationID,date,prettyDate,parameter) %>% # Subset parameter of interest from whole dataset
    dplyr::rename(parameter2=!!names(.[4])) %>% # sneaky rename so ggplot will play nicely in function with changeable parameter variable
    filter(!is.na(parameter2)) # Remove samples without parameter measurements
  
  # output plot if there is data, if not tell user
  if(nrow(dat)==0){
    return(print("No data for that parameter in the input dataset."))
  }else{
    
    # Find min and max dates based on dataset to use for annotation rectangle limits in ggplot
    minDate <- min(dat$date)-15
    maxDate <- max(dat$date)+15
    
    # trick datebreaks to plot if only 1 point given for that parameter
    datebreaks <- ifelse(nrow(dat)>1,datebreaks,'3 week')
    
    if(parameter== 'DO (mg/L)'){specialSettings <- DOsettingsThresholds}
    if(parameter=='pH (unitless)'){specialSettings <- pHsettings}
    if(parameter=="SpCond (uS/cm)"){specialSettings <- spCondsettings}
    if(parameter=="TDS (mg/L)"){specialSettings <- TDSsettings}
    if(parameter=='DSulfate'){specialSettings <- Sulfatesettings}
    if(parameter=='DChloride'){specialSettings <- Chloridesettings}
    if(parameter=='DPotassium'){specialSettings <- Potassiumsettings}
    if(parameter=='DSodium'){specialSettings <- Sodiumsettings}
    if(parameter=="TN (mg/L)"){specialSettings <- TNsettings}
    if(parameter=='TP'){specialSettings <- TPsettings}
    if(parameter=='Total Habitat (unitless)'){specialSettings <- TotHabsettings}
    if(parameter=="LRBS (unitless)"){specialSettings <- LRBSsettings}
    if(parameter=='Temp'){specialSettings <- Tempsettings}
    if(parameter=='MetalsCCU'){specialSettings <- MetalsCCUsettings}
    if(parameter=='FamSCI'){specialSettings <-VSCIsettings}
    
    # Adjust plot limits if data are outside specialSettings preset range
    if(max(dat$parameter2) >= specialSettings[[length(specialSettings)-1]][['limits']][2]){
      specialSettings[[length(specialSettings)-1]][['limits']][2] <- signif(max(dat$parameter2)*1.15,2)
      specialSettings[[length(specialSettings)-1]][['breaks']] <- signif(seq(min(specialSettings[[length(specialSettings)-1]][['breaks']]),
                                                                             specialSettings[[length(specialSettings)-1]][['limits']][2],length.out=10),2)}
    
    # Now make the plot
    p <- ggplot(dat, aes(x=date, y=parameter2, group=StationID)) + 
      theme(axis.text=element_text(size=14, face="bold"), legend.text=element_text(size=14),legend.title =   element_text(size=14, face="bold"),axis.title=element_text(size=14, face="bold")) +
      scale_x_date(date_breaks = datebreaks, date_labels =  "%b-%y") + 
      coord_cartesian(xlim = as.Date(c(maxDate, minDate))) +
      theme(axis.text.x=element_text(angle=45,hjust=1)) +  
      
      specialSettings + 
      
      geom_point(aes(fill=StationID),colour='black',shape=21, size=6) +
      scale_fill_manual(values=gray.colors(length(unique(dat$StationID))))+  # have different point options depending on number of stations
      guides(fill=guide_legend(title="Station ID")) 
    return(p)
  }
}


# Risk table
riskHightoLow <- c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')
risk <- data.frame(Risk_Category=c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life'))
brksrisk <- c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')
clrsrisk <- c("firebrick","#F0E442","#009E73","#0072B2")

# Each parameter risk table and colors
pHRiskTable <- list(
  Data = data.frame(Risk_Category=c('Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life'),pH=c("< 6","6 - 9","> 9")),
  ColNames = c('Risk Category','pH (unitless)'),
  StyleEqual1 = c('Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life'),
  StyleEqual2 = c('#F0E442','#009E73'),
  brks = c(0,6,9),
  clrs = c("gray","#F0E442","#009E73","#F0E442")
)

DORiskTable <- list(
  Data = cbind(risk,DO=c('< 7','> 7, < 8','> 8, < 10','> 10')),
  ColNames = c('Risk Category','DO (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,7,8,10),
  clrs = c("gray","firebrick","#F0E442","#009E73","#0072B2")
)

TNRiskTable <- list(
  Data = cbind(risk,TN=c('> 2','> 1, < 2','> 0.5, < 1','< 0.5')),
  ColNames = c('Risk Category','Total Nitrogen (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,0.5,1,2),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

TPRiskTable <- list(
  Data = cbind(risk,TP=c('> 0.1','> 0.05, < 0.1','> 0.02, < 0.05','< 0.02')),
  ColNames = c('Risk Category','Total Phosphorus (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,0.02,0.05,0.1),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

TotHabRiskTable <- list(
  Data = cbind(risk,TotalHabitat=c('< 100','> 100, < 130','> 130, < 150','> 150')),
  ColNames = c('Risk Category','Total Habitat (unitless)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,100,130,150),
  clrs = c("gray","firebrick","#F0E442","#009E73","#0072B2")
)

LRBSRiskTable <- list(
  Data = cbind(rbind(risk,'Medium Probability of Stress to Aquatic Life'),LRBS=c('< -1.5','> -1.5, < -1.0','> -0.5, < -1.0','> -0.5, < 0.5','> 0.5')),
  ColNames = c('Risk Category','Relative Bed Stability (unitless)'),
  StyleEqual1 = c(riskHightoLow,'Medium Probability of Stress to Aquatic Life'),
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2","#F0E442"),
  brks = c(-3,-1.5,-1,-0.5,0.5),
  clrs = c("gray","firebrick","#F0E442","#009E73","#0072B2","#F0E442")
)

MetalsCCURiskTable <- list(
  Data = cbind(risk,MetalsCCU=c('> 2.0','> 1.5, < 2.0','> 0.75, < 1.5','< 0.75')),
  ColNames = c('Risk Category','Metals CCU (unitless)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,0.75,1.5,2.0),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

SpCondRiskTable <- list(
  Data = cbind(risk,SpCond=c('> 500','> 350, < 500','> 250, < 350','< 250')),
  ColNames = c('Risk Category','Specific Conductivity (uS/cm)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,250,350,500),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

TDSRiskTable <- list(
  Data = cbind(risk,TDS=c('> 350','> 250, < 350','> 100, < 250','< 100')),
  ColNames = c('Risk Category','Total Dissolved Solids (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks =  c(0,100,250,350),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DSulfateRiskTable <- list(
  Data = cbind(risk,DSulfate=c('> 75','> 25, < 75','> 10, < 25','< 10')),
  ColNames = c('Risk Category','Dissolved Sulfate (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,10,25,75),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DChlorideRiskTable <- list(
  Data = cbind(risk,DChloride=c('> 50','> 25, < 50','> 10, < 25','< 10')),
  ColNames = c('Risk Category','Dissolved Chloride (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks =  c(0,10,25,50),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DPotassiumRiskTable <- list(
  Data = cbind(risk,DPotassium=c('> 10','> 2, < 10','> 1, < 2','< 1')),
  ColNames = c('Risk Category','Dissolved Potassium (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,1,2,10),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DSodiumRiskTable <- list(
  Data = cbind(risk,DSodium=c('> 20','> 10, < 20','> 7, < 10','< 7')),
  ColNames = c('Risk Category','Dissolved Sodium (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks =  c(0,7,10,20),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)


brks <- 1:19
clrs <- c("#8B0000", "#9D0000", "#AF0000", "#C10000", "#D40000", "#E60000", "#F80000", "#FF1415", "#FF3235", "#FF5055", "#FF6F75",
          "#FF8D95", "#FFABB5", "#FFC3CD", "#FFCDD5", "#FFD7DE", "#FFE1E6", "#FFEBEE", "#FFF5F6", "#FFFFFF")

lowGradientHabitatHeatmap <- function(habData){
  if("POOLSUB" %in% unique(habData$HabParameter) | 'POOLVAR' %in% unique(habData$HabParameter) ){ 
    # Low Gradient Habitat Method
    habData$CollDate <- as.Date(as.character(habData$CollDate),format="%Y-%m-%d")
    totHabData <- select(habData,StationID,CollDate,TotHabSc)[!duplicated(select(habData,StationID,CollDate,TotHabSc)),] # get unique sample dates
    habData1 <- select(habData,-c(TotHabSc)) %>% # these two fields can cause multiple rows for each date in the spread() step, so get rid of them for now
      filter(HabParameter %in% c("ALTER","BANKS","BANKVEG","FLOW","POOLSUB","POOLVAR","RIPVEG","SEDIMENT","SINUOSITY",
                                 "SUBSTRATE","COVER")) %>% # get only low gradient parameters in case both happened to be sampled for site) and using filter is safer than select at a later step
      spread(HabParameter,HabValue) %>%
      filter(!is.na(POOLSUB) & !is.na(POOLVAR) & !is.na(SINUOSITY)) # if both methods used then get rid of sample dates not sampled as low gradient
    
    # Deal with COVER terminology if present
    if("COVER" %in% names(habData1)){
      habData1 <- habData1 %>% rowwise()%>%
        mutate(SUBSTRATE=sum(COVER,SUBSTRATE,na.rm=T)) %>% select(-c(COVER)) %>% # Get rid of old cover terminology
        left_join(totHabData, by=c("StationID","CollDate")) %>%
        dplyr::rename(TotalHabitat = TotHabSc) %>%
        arrange(CollDate)
    }else{
      habData1 <- habData1 %>% rowwise()%>%
        left_join(totHabData, by=c("StationID","CollDate")) %>%
        dplyr::rename(TotalHabitat = TotHabSc) %>%
        arrange(CollDate)
    }
    
    return(DT::datatable(habData1,escape=F, rownames = F, 
                         
                         colnames = c('Station ID','Date','Channel Alteration','Bank Stability','Bank Vegetation', 
                                      'Flow', 'Pool Substrate','Pool Variability', 'Riparian Vegetation', 
                                      'Sediment', 'Sinuosity', 'Substrate', 'Total Habitat'),
                         options=list(pageLength=nrow(habData1),dom= 'Bt' )) %>%
             formatStyle(names(habData1)[3:12], backgroundColor = styleInterval(brks, clrs), 
                         textAlign = 'center', `font-family` = 'Arial') %>%
             formatStyle(names(habData1), fontWeight = styleInterval(10, c('bold','normal')), 
                         textAlign = 'center', `font-family` = 'Arial') %>%
             formatStyle('TotalHabitat', backgroundColor = "lightgray"))
  }
}
highGradientHabitatHeatmap <- function(habData){
  if("RIFFLES" %in% unique(habData$HabParameter) | "VELOCITY" %in% unique(habData$HabParameter) ){ # High Gradient Habitat Method
    habData$CollDate <- as.Date(as.character(habData$CollDate),format="%Y-%m-%d")
    totHabData <- select(habData,StationID,CollDate,TotHabSc)[!duplicated(select(habData,StationID,CollDate,TotHabSc)),] # get unique sample dates
    habData1 <- select(habData,-c(TotHabSc)) %>% # these two fields can cause multiple rows for each date in the spread() step, so get rid of them for now
      filter(HabParameter %in% c('ALTER','BANKS','BANKVEG','COVER','EMBED','FLOW','RIFFLES','RIPVEG','SEDIMENT',
                                 'SUBSTRATE','VELOCITY')) %>% # get only high gradient parameters in case both happened to be sampled for site) and using filter is safer than select at a later step
      spread(HabParameter,HabValue) %>%
      filter(!is.na(EMBED) & !is.na(RIFFLES) & !is.na(VELOCITY)) # if both methods used then get rid of sample dates not sampled as high gradient
    
    # Deal with COVER terminology if present
    if("COVER" %in% names(habData1)){
      habData1 <- habData1 %>% rowwise()%>%
        mutate(SUBSTRATE=sum(COVER,SUBSTRATE,na.rm=T)) %>% select(-c(COVER)) %>% # Get rid of old cover terminology
        left_join(totHabData, by=c("StationID","CollDate")) %>%
        dplyr::rename(TotalHabitat = TotHabSc) %>%
        arrange(CollDate)
    }else{
      habData1 <- habData1 %>% rowwise()%>%
        left_join(totHabData, by=c("StationID","CollDate")) %>%
        dplyr::rename(TotalHabitat = TotHabSc) %>%
        arrange(CollDate)
    }
    return(DT::datatable(habData1,escape=F, rownames = F, 
                         colnames = c('Station ID','Date','Channel Alteration','Banks', 
                                      'Bank Vegetation', 'Embeddedness', 
                                      'Flow', 'Riffles', 'Riparian Vegetation', 
                                      'Sediment', 'Substrate','Velocity', 'Total Habitat'),
                         options=list(pageLength=nrow(habData1),dom= 'Bt' )) %>%
             formatStyle(names(habData1)[3:12], backgroundColor = styleInterval(brks, clrs), 
                         textAlign = 'center', `font-family` = 'Arial') %>%
             formatStyle(names(habData1), fontWeight = styleInterval(10, c('bold','normal')), 
                         textAlign = 'center', `font-family` = 'Arial') %>%
             formatStyle('TotalHabitat', backgroundColor = "lightgray"))
    
    
  }
}


removeUnits_envDataDF <- function(envData){
  return(rename(envData,  pH = `pH (unitless)`, DO = `DO (mg/L)` , TN =  `TN (mg/L)`, 
                TP = `TP (mg/L)`, TotalHabitat = `Total Habitat (unitless)`,
                LRBS = `LRBS (unitless)` , MetalsCCU = `MetalsCCU (unitless)`,
                SpCond =`SpCond (uS/cm)`,  TDS = `TDS (mg/L)` ,  
                DSulfate = `DSulfate (mg/L)` , DChloride= `DChloride (mg/L)`, 
                DPotassium = `DPotassium (mg/L)`, DSodium = `DSodium (mg/L)`,
                Temp = `Temperature (C)`))
}
