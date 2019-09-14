##############################
# StackedAreaFunLibrary      #
# -------------------------- #
# Last updated on: 12/5/2018 #
# Last updated by: rlp       #
##############################

###
#Convert a Catch Matrix to a Catch Data Frame
###
#inputs:
#x - input matrix of auxiliary catch information in the layout of 
	#column 1 year, 
	#column 2:n catch by stock, 
	#column name is numeric, corresponding to the model fishery number
#outputs:
#out - a dataframe of the aux catch data in the layout used elsewhere
ConvertCatchMat <- function(x) {
	#note that the format of aux cat matrix is year, fishery, and obs_catch_nom
	fnames = as.numeric(substr(names(x)[2:ncol(x)],2,3)) #fishery names reformatted - R adds an "X" to numeric variables
	out = data.frame(year = rep(matrix(as.matrix(x[,1])),ncol(x)-1),
	                 fishery = matrix(sapply(fnames, rep, nrow(x))),
	                 obs_catch_nom = matrix(as.matrix(x[,2:ncol(x)])))
	return(out)
  #OLD CODE BELOW
  #if the number of columns greater than 3, implies format 2
  #NOTE, routine ASSUMES fisheries are in numerical order. a must!
    #nauxfish = ncol(auxcat)-1
    #fnumbers = substring(colnames(auxcat)[2:ncol(auxcat)],2,4)
    #fnumbers = sort(rep(as.numeric(fnumbers), nrow(auxcat)))
    #years = rep(auxcat$Year, length(2:ncol(auxcat)))
    #temp = data.frame(year=years, fishery=fnumbers)
    #temp$obs_catch_nom = do.call(c, auxcat[,-1])
    #auxcat = temp
}


###
#StackedAreaData
###
#inputs:
 #stockmap
 #fisherymap
 #auxcat
 #obscat
 #aabmcat
 #isbmcat
 #write_output - to save a file, specify the file name here. ELSE NULL and do nothing. 
#outputs:
 #DataToR - a dataframe of the aux catch data in the layout used elsewhere


StockCompData <- function(stockmap, fisherymap, auxcat, obscat, aabmcat, isbmcat, write_output=NULL, maxyear=NULL, minyear=NULL, seakaddon=NULL) {
 #model fishery
  modfishery = fisherymap[c("fishery","fisheryNameLong")]
  #observed catch data (2 data formats)
  #if the number of columns greater than 3, implies format 2
  #NOTE, routine ASSUMES fisheries are in numerical order. a must!
  if(ncol(auxcat)>3) { #test if format is if #of columns is greater than 2
   auxcat = ConvertCatchMat(auxcat)
  }
  #prelims - NOTE, i'm truncating model catch to observed catch (hence projection 'catch' is truncated)
  if(is.null(minyear)) minyear = max(min(auxcat$year),min(obscat$year),min(aabmcat$year),min(isbmcat$year))
  if(is.null(maxyear)) maxyear = min(max(auxcat$year),max(obscat$year),max(aabmcat$year),max(isbmcat$year))
 #combine aux cat and obs cat (data from the CM) into one file
  allobscat = rbind(auxcat, obscat)
  allobscat = allobscat[ order(allobscat$year, allobscat$fishery), ]
  allobscat = subset(allobscat, year<=maxyear&year>=minyear)
 #model stock comp calculations
  #format model catch data
   allcat = rbind(aabmcat, isbmcat) #combine isbm and aabm fishery catch
   allcat$nomcat = rowSums(allcat[,5:6]) # Term.catch + preTerm.catch
   allcat$totcat = rowSums(allcat[,5:20]) # The kitchen sink
   allcat = subset(allcat, year<=maxyear&year>=minyear)
  #add stock comp mappings, similar to a 'vlookup' 
   allcat=merge(stockmap, allcat, by = "stock") # Append stock mapping to data frame
  #calculate stock comp of catch by 'whole' fish
   stockcomp.nom.lc.numeric = with(allcat, tapply(nomcat, list(year, fishery, stock.group.order), sum))
  #calculate stock comp of model catch by proportion in catch, with special logic if there's no model catch (e.g. NA handling)
   stockcomp.nom.lc.percent = stockcomp.nom.lc.numeric
   for(i in 1:dim(stockcomp.nom.lc.percent)[2]) stockcomp.nom.lc.percent[,i,] = ifelse(stockcomp.nom.lc.numeric[,i,]==0, 0, stockcomp.nom.lc.numeric[,i,]/rowSums(stockcomp.nom.lc.numeric[,i,]))
 #convert data to the r format & calculate/append catch
  #format: 	FisheryName	FisheryNameLong	Year	StockGroup	Catch	PropinCatch
  #prelims
   nfisheries   = nrow(modfishery)
   nstockgroups = ncol(stockcomp.nom.lc.percent[,1,])
   nyears       = nrow(stockcomp.nom.lc.percent[,1,])
   minyear      = min(allcat$year)
  #FisheryName and FisheryNameLong
   DataToR = data.frame(fishery = sort(rep(modfishery$fishery, nyears*nstockgroups)))
   DataToR = merge(modfishery, DataToR, by="fishery")
  #Year
   DataToR$year = rep(sort(rep(minyear:maxyear, nstockgroups)), nfisheries)
  #StockGroup
   temp = sort(unique(stockmap$stock.group.order))
   temp = stockmap[match(temp, stockmap$stock.group.order),]$stock.group
   DataToR$stock = rep(temp, nfisheries*nyears)
  #Catch
   DataToR = merge(DataToR, allobscat, by=c("year","fishery"), sort=FALSE)
  #PropinCatch
   DataToR$catch = NA
   DataToR$pcatch = NA
   DataToR$modcatch = NA
   DataToR$mod_catch_nom = NA
   DataToR$preferred_catch = NA
   DataToR$preferred_catch_nom = NA
   DataToR$preferred_catch_type = NA
   index = 1:nstockgroups
   for(i in 1:nfisheries) {
     for(j in 1:nyears) {
       DataToR[index,]$modcatch = stockcomp.nom.lc.numeric[j,i,]
       DataToR[index,]$mod_catch_nom = sum(stockcomp.nom.lc.numeric[j,i,])
       DataToR[index,]$pcatch = stockcomp.nom.lc.percent[j,i,]
       DataToR[index,]$catch = DataToR[index,]$obs_catch_nom*stockcomp.nom.lc.percent[j,i,]
       DataToR[index,]$preferred_catch = ifelse(rep(sum(DataToR[index,]$obs_catch_nom<0),nrow(DataToR[index,])), DataToR[index,]$modcatch, DataToR[index,]$catch)
       DataToR[index,]$preferred_catch_nom = ifelse(sum(DataToR[index,]$obs_catch_nom)<0, DataToR[index,]$mod_catch_nom, DataToR[index,]$obs_catch_nom)
       DataToR[index,]$preferred_catch_type = ifelse(sum(DataToR[index,]$obs_catch_nom)<0, "Model Catch", "Observed Catch")
       index = index + nstockgroups
     }
   }
   ################################
   #Start of SEAK Addon adjustment#
   ################################
   if(!is.null(seakaddon)) {
     #SEAK catch data (2 data formats)
     #if the number of columns greater than 3, implies format 2
     #NOTE, routine ASSUMES fisheries are in numerical order. a must!
     if(ncol(seakaddon)>3) {
       nauxfish = ncol(seakaddon)-1
       fnumbers = substring(colnames(seakaddon)[2:ncol(seakaddon)],2,4)
       fnumbers = sort(rep(as.numeric(fnumbers), nrow(seakaddon)))
       years = rep(seakaddon$Year, length(2:ncol(seakaddon)))
       temp = data.frame(year=years, fishery=fnumbers)
       temp$stock = "SEAK"
       temp$addon = do.call(c, seakaddon[,-1])
       seak_addon = temp
       faux = sort(unique(fnumbers))
     }
     #Update "catch" field
     DataToR[rownames(subset(DataToR, fishery==faux[1]&stock=="SEAK")),]$catch = subset(DataToR, fishery==faux[1]&stock=="SEAK")$catch + subset(seak_addon,fishery==faux[1])$addon
     DataToR[rownames(subset(DataToR, fishery==faux[2]&stock=="SEAK")),]$catch = subset(DataToR, fishery==faux[2]&stock=="SEAK")$catch + subset(seak_addon,fishery==faux[2])$addon
     DataToR[rownames(subset(DataToR, fishery==faux[3]&stock=="SEAK")),]$catch = subset(DataToR, fishery==faux[3]&stock=="SEAK")$catch + subset(seak_addon,fishery==faux[3])$addon
     #Update "obs_catch_nom" field
     DataToR[rownames(subset(DataToR, fishery==faux[1])),]$obs_catch_nom = subset(DataToR, fishery==faux[1])$obs_catch_nom + as.vector(mapply(rep, subset(seak_addon,fishery==faux[1])$addon, nstockgroups))
     DataToR[rownames(subset(DataToR, fishery==faux[2])),]$obs_catch_nom = subset(DataToR, fishery==faux[2])$obs_catch_nom + as.vector(mapply(rep, subset(seak_addon,fishery==faux[2])$addon, nstockgroups))
     DataToR[rownames(subset(DataToR, fishery==faux[3])),]$obs_catch_nom = subset(DataToR, fishery==faux[3])$obs_catch_nom + as.vector(mapply(rep, subset(seak_addon,fishery==faux[3])$addon, nstockgroups))
     #Update "pcatch" field
     DataToR$pcatch =  DataToR$catch/DataToR$obs_catch_nom
     #Update "preferred_catch" field
     DataToR[rownames(subset(DataToR, fishery==faux[1]&stock=="SEAK")),]$preferred_catch = subset(DataToR, fishery==faux[1]&stock=="SEAK")$preferred_catch + subset(seak_addon,fishery==faux[1])$addon
     DataToR[rownames(subset(DataToR, fishery==faux[2]&stock=="SEAK")),]$preferred_catch = subset(DataToR, fishery==faux[2]&stock=="SEAK")$preferred_catch + subset(seak_addon,fishery==faux[2])$addon
     DataToR[rownames(subset(DataToR, fishery==faux[3]&stock=="SEAK")),]$preferred_catch = subset(DataToR, fishery==faux[3]&stock=="SEAK")$preferred_catch + subset(seak_addon,fishery==faux[3])$addon
     #Update "preferred_catch_nom" field
     DataToR[rownames(subset(DataToR, fishery==faux[1])),]$preferred_catch_nom = subset(DataToR, fishery==faux[1])$preferred_catch_nom + as.vector(mapply(rep, subset(seak_addon,fishery==faux[1])$addon, nstockgroups))
     DataToR[rownames(subset(DataToR, fishery==faux[2])),]$preferred_catch_nom = subset(DataToR, fishery==faux[2])$preferred_catch_nom + as.vector(mapply(rep, subset(seak_addon,fishery==faux[2])$addon, nstockgroups))
     DataToR[rownames(subset(DataToR, fishery==faux[3])),]$preferred_catch_nom = subset(DataToR, fishery==faux[3])$preferred_catch_nom + as.vector(mapply(rep, subset(seak_addon,fishery==faux[3])$addon, nstockgroups))
     }
  ##############################
  #End of SEAK Addon adjustment#
  ##############################
  #Re-order and drop superfolous columns
   DataToR = DataToR[c("fishery","fisheryNameLong","fisheryNameLong","year","stock","catch","pcatch","modcatch","mod_catch_nom","preferred_catch","preferred_catch_nom","preferred_catch_type")]
   names(DataToR) = c("FisheryNum","FisheryName","FisheryNameLong","Year","StockGroup","Catch","PropinCatch","ModelCatch","ModelCatchTotal","PreferredCatch","PreferredTotalCatch","PreferredCatchType")
  #write output if so desired
   if(!is.null(write_output)) {
    write.table(x=DataToR, file=write_output, sep="\t", row.names=FALSE)
   }
  return(DataToR)
}


###########
#CatchData#
###########
#description
 #function manipulates data 
#inputs:
 #fisherymap
 #auxcat
 #obscat
 #aabmcat
 #isbmcat
 #write_output - to save a file, specify the file name here. ELSE NULL and do nothing. 
#outputs:
 #DataToR - a dataframe of the aux catch data in the layout used elsewhere
#
CatchData <- function(fisherymap, auxcat, obscat, aabmcat, isbmcat, write_output = NULL) {
 #model fishery
  modfishery = fisherymap[c("fishery","fisheryNameLong","fishery.group")]
 #observed catch data comes in 2 formats.
 #the program only takes one format, so manipulate format 2 to format 1
  if(ncol(auxcat)>3) { #test if format is if #of columns is greater than 2
   auxcat = ConvertCatchMat(auxcat)
  }
 #combine aux cat and obs cat (data from the CM) into one file
  allobscat = rbind(auxcat, obscat)
  allobscat = allobscat[ order(allobscat$year, allobscat$fishery), ]
 #model stock comp calculations
  #prelims - NOTE, i'm truncating model catch to observed catch (hence projection 'catch' is truncated)
   minyear = min(allobscat$year)
   maxyear = max(allobscat$year) 
  #format model catch data
   allcat = rbind(aabmcat, isbmcat) #combine isbm and aabm fishery catch
   allcat$nomcat = rowSums(allcat[,5:6]) # Term.catch + preTerm.catch
   allcat$totcat = rowSums(allcat[,5:20]) # The kitchen sink
   allcat = subset(allcat, year <= maxyear)
  #calculate catch by fishery and type
   nomcat = with(allcat, tapply(nomcat, list(year, fishery), sum))
   totcat = with(allcat, tapply(totcat, list(year, fishery), sum))
  #convert nomcat and totcat
   nomcat2 = ConvertCatchMat(data.frame(year=as.numeric(rownames(nomcat)),data.frame(nomcat)))
   totcat2 = ConvertCatchMat(data.frame(year=as.numeric(rownames(totcat)),data.frame(totcat)))
   names(nomcat2)[3] = "nomcat"
   names(totcat2)[3] = "totcat"
 #convert data to the r format & calculate/append catch
  #format: 	FisheryName	FisheryNameLong	Year	StockGroup	Catch	PropinCatch
  #prelims
   nfisheries   = nrow(modfishery)
   nyears       = nrow(nomcat)
  #FisheryName and FisheryNameLong
   DataToR = data.frame(fishery = sort(rep(modfishery$fishery, nyears)))
   DataToR = merge(modfishery, DataToR, by="fishery")
  #Year
   DataToR$year = rep(sort(rep(minyear:maxyear)), nfisheries)
  #Catch
   DataToR = merge(DataToR, allobscat, by=c("year","fishery"), sort=FALSE)
   DataToR = merge(DataToR, nomcat2, by=c("year","fishery"), sort=FALSE)
   DataToR = merge(DataToR, totcat2, by=c("year","fishery"), sort=FALSE)
  #Re-order and drop superfolous columns
   DataToR = DataToR[c("fishery","fisheryNameLong","fishery.group","year","obs_catch_nom","nomcat","totcat")]
   names(DataToR) = c("FisheryNum","FisheryName","FisheryGroup","Year","Observed_Catch","Model_LCatch","Model_TM")
  #write output if so desired
   if(!is.null(write_output)) {
    write.table(x=DataToR, file=write_output, sep="\t", row.names=FALSE)
   }
  return(DataToR)
}


#######
#StackedAreaFigures
######
#description
 #This program is designed to create stacked area time series figures of Chinook-model derived
 #stock composition of actual landed catch; the R code is fairly straightforward; however, the 
 #prep of modeloutput as needed for the program requires additional work, spearheaded by Gayle in 2012
 #NOTE: This is another somewhat painful use of ggplot, but it gets it done as needed...
 #      ALSO, be sure to relabel as needed the APPENDIX LETTER AND YEAR associated with captions, 
 #      which are written into image files here as a matter of convenience/function
 #code from pete m, the original CTC R using badass
#inputs
 #
#outputs
 #
StackedAreaFigures <- function(x, whichstocks = NULL, ifpdf = FALSE, out_dir = NULL, caps_on = FALSE) {
#set directory
if(!is.null(out_dir)) {
 odir = getwd()
 setwd(out_dir)
}
#Load necessary packages
require(ggplot2)
require(gridExtra)

#load data
 modcomp = x
 #modcomp = read.table(modcompfilename,header=T)

figs = max(modcomp$FisheryNum)
i = 1

#This can be written into a single multi-page pdf OR as multiple individual jpgs; 
#the latter imports readily into MS Word but is sloppier for transport/other uses perhaps
#if doing pdf option, uncomment (1) the pdf line, (2) the print line and (3) the dev.off()
#************************************************************************************
if(ifpdf) pdf(file=paste("ModStockCompCLB", substr(maxyear+1,3,4),"XX.pdf",sep=""),height=8,width=13)
while(i<=figs)
{
 #take subset of data for each plot
  sub1<-subset(modcomp,modcomp$FisheryNum==i)
 #set breaks and colors depending on whichstocks
  #note that if the number of groups is greater then brewer.pal will give a warning
  if(is.null(whichstocks)) {
    mybreaks = levels(sub1$StockGroup)
    mycols = brewer.pal(length(mybreaks),"Set3")
  } else if (whichstocks=="9806") {
    mybreaks = c('ORCST','CR-tule','CR-sp&su','CR-bright','WACST','PSD','FR-late','FR-early','GS','WCVI','NCBC','SEAK')
    mycols = brewer.pal(length(mybreaks),"Set3")
    sub1$StockGroup = factor(sub1$StockGroup, levels=mybreaks) #re-order stock groups
    mycols = mycols[c(8,3,2,1,11,9,5,4,6,12,7,10)] #re-order colors manually to match old reports (dunno why it changed, something to do with R)
  } else if (whichstocks=="phase2") {
    cat("phase 2 list for these figures has not been formally discussed, program will defaults\n")
    mybreaks = levels(sub1$StockGroup)
    mycols = brewer.pal(length(mybreaks),"Set3")
  }
   else { 
    cat("whichstocks definition not recognized, program will use defaults\n") 
    mybreaks = levels(sub1$StockGroup)
    mycols = brewer.pal(length(mybreaks),"Set3")
   }
    
    #determine while running each fishery what the axis tick breaks should be
    lim<-max(tapply(sub1$PreferredCatch,sub1$Year,sum))
    if(lim <= 50000)
    {tickseq = seq(0,50,5)} else
      if(lim >= 1000000)
      {tickseq = seq(0,round(lim/1000,-2),l=5)} else
      if(lim >= 300000)
      {tickseq = seq(0,1000,50)} else
        if(lim >= 200000)
        {tickseq = seq(0,300,30)} else
          if(lim >100000)
          {tickseq = seq(0,200,20)} else
            {tickseq = seq(0,100,10)}
      
    #the figure meat
    image<-ggplot(sub1, aes(x=sub1$Year, y=sub1$PreferredCatch/1000, fill=sub1$StockGroup)) + 
      geom_area(colour="black", size=.2, alpha=1) + 
      #scale_fill_brewer(palette="Set3",name="Stock Group", breaks=mybreaks) + 
      scale_fill_manual(values=mycols,  name="Stock Group") +
      theme(axis.title.x = element_text(size=20),axis.text.x  = element_text(angle=0, size=15,color="black", hjust=1, vjust=1)) +
      theme(axis.title.y = element_text(size=20),axis.text.y  = element_text(angle=0, size=15,color="black")) +
      labs(title = as.character(sub1$FisheryNameLong[1])) +
      theme(plot.title = element_text(size = rel(2))) +
      ylab("Catch (thousands)")+xlab("")+
      scale_x_continuous(expand=c(0,0),breaks=seq(1980,maxyear,5))+
      scale_y_continuous(expand=c(0,0),breaks=tickseq)+
      theme(legend.title = element_text(size=15, face="bold"))+
      theme(legend.text = element_text(size = 15))+
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
      theme(panel.background = element_blank())+ #removes the gray filled back
      theme(panel.border = element_rect(fill=NA,linetype = "solid",colour = "black")) #Adds a border to plot 
      if (caps_on==TRUE) { 
         xx<-as.character(sub1$FisheryNameLong)[1]
         Caption<-paste("Appendix E",i,"  Chinook Model estimates of landed catch stock composition for ", xx," 1979-", max(sub1$Year), sep="")
       image <- arrangeGrob(image,sub=textGrob(Caption,x = 0, hjust = 0, vjust=0.1,
                                              gp = gpar( fontsize = 14)))
      } else {
        image <- arrangeGrob(image)
      }
       # the above bit adds a caption to the figures
      #now save each one
      #Sloppy exception handling since file names can't have "\" in them      
      fname<-as.character(sub1$FisheryNameLong)[1]
      if(fname=="Washington/Oregon Troll")
        {fname<-"WashingtonOregon Troll"}
      if(fname=="North/Central BC Sport")
        {fname<-"NorthCentral BC Sport"}  
    
    ggsave(paste(i,fname,".jpg",sep=""),plot=image, width=13, height=8,dpi=800)
    
    if(ifpdf) print(image)
    
    i = i+1
}
dev.off()
#************************************************************************************
setwd(odir)
}

#######
#StackedAreaFigure
######
#description
 #This program is designed to create stacked area time series figures of Chinook-model derived
 #stock composition of actual landed catch; the R code is fairly straightforward; however, the 
 #prep of modeloutput as needed for the program requires additional work, spearheaded by Gayle in 2012
 #NOTE: This is another somewhat painful use of ggplot, but it gets it done as needed...
 #      ALSO, be sure to relabel as needed the APPENDIX LETTER AND YEAR associated with captions, 
 #      which are written into image files here as a matter of convenience/function
 #code from pete m, the original CTC R using badass
#inputs
 #
#outputs
 #
#StackedAreaFigure(x=CLB1804_stk_seakaddon, fishnum = 3, caps_on = FALSE, labs_title="CLB1804")

StackedAreaFigure <- function(x, fishnum = 1, caps_on = FALSE, minyear = NULL, maxyear = NULL, labs_title = NULL, gsicompare = FALSE, whichstocks = NULL) {
#Load necessary packages
  suppressMessages(require(ggplot2))
  suppressMessages(require(gridExtra))
  suppressMessages(require(grid))
  suppressMessages(require(RColorBrewer))
#load data
 modcomp = x
#set fishery number to plot
 i = fishnum
#determine max and min year (if not specified by user)
 if(is.null(minyear)) minyear = min(modcomp$Year)
 if(is.null(maxyear)) maxyear = max(modcomp$Year)
#subset of data for each plot
 sub1<-subset(modcomp,FisheryNum==i&Year<=maxyear&Year>=minyear)
#set breaks to the current order (south-to-north)
 #mybreaks = c("CR-bright","CR-sp&su","CR-tule","FR-early","FR-late","GS","NCBC","ORCST","PSD","SEAK","WACST","WCVI") #old color order
 
 #note that if the number of groups is greater then brewer.pal will give a warning
 #set breaks to the current order (south-to-north)
 if(is.null(whichstocks)) {
   #mybreaks = c("CR-bright","CR-sp&su","CR-tule","FR-early","FR-late","GS","NCBC","ORCST","PSD","SEAK","WACST","WCVI") #old color order
   mybreaks = c('ORCST','CR-tule','CR-sp&su','CR-bright','WACST','PSD','FR-late','FR-early','GS','WCVI','NCBC','SEAK')
   #mybreaks = c('AK','Canada','FR-late','FR-early','SUS')
 } else {
   mybreaks = whichstocks
 }
 
 mycols = brewer.pal(length(mybreaks),"Set3")
 #mycols[11] = ""
 
 if(gsicompare) {
   if(is.null(whichstocks)) {
     #redefine 'breakpoints'
     mybreaks = c('ORCST','CR','WACST','PSD','FR-late','FR-early','GS','WCVI','NCBC','SEAK', 'NONE', 'UNKNOWN')
     #redfine the colors (noting that CR stocks were collapsed to a single group)
     mycols = c(mycols[1:2], mycols[5:length(mycols)], "black", "white")
   } else {
     mybreaks = whichstocks
   }
   
   #if the 'none' category DOES not exist in the original data, add it!
    if(!("NONE" %in% levels(sub1$StockGroup))) { 
      tmp = subset(sub1, StockGroup==levels(sub1$StockGroup)[1])
      tmp$StockGroup = "NONE"
      tmp$PreferredCatch = 0
      tmp$PropinCatch = 0
      sub1 = rbind(sub1,tmp)
    }
    if(!("UNKNOWN" %in% levels(sub1$StockGroup))) { 
      tmp = subset(sub1, StockGroup==levels(sub1$StockGroup)[1])
      tmp$StockGroup = "UNKNOWN"
      tmp$PreferredCatch = 0
      tmp$PropinCatch = 0
      sub1 = rbind(sub1,tmp)
    }
 }
 
 sub1$StockGroup2 = factor(as.character(sub1$StockGroup), levels = mybreaks)
 
   #determine title of figure (if not specified by user)
 if(is.null(labs_title)) labs_title = as.character(sub1$FisheryNameLong[1])
 
#determine while running each fishery what the axis tick breaks should be
    lim<-max(tapply(sub1$PreferredCatch,as.factor(sub1$Year),sum))
    if(lim <= 50000)
    {tickseq = seq(0,50,5)} else
      if(lim >= 1000000)
      {tickseq = seq(0,round(lim/1000,-2),l=5)} else
      if(lim >= 300000)
      {tickseq = seq(0,1000,50)} else
        if(lim >= 200000)
        {tickseq = seq(0,300,30)} else
          if(lim >100000)
          {tickseq = seq(0,200,20)} else
          {tickseq = seq(0,100,10)}
#determine the x-axis ticks
    if((maxyear - minyear)>=20) xtickseq = seq(1980,maxyear,5)
    if((maxyear - minyear)<20) xtickseq = seq(minyear,maxyear,by=3)
    if((maxyear - minyear)<10) xtickseq = seq(minyear,maxyear,by=1)
    
    levels(sub1$StockGroup2)   
#create figure
    image<-ggplot(sub1, aes(x=sub1$Year, y=sub1$PreferredCatch/1000, fill=sub1$StockGroup2)) +
      geom_area(colour="black", size=.2, alpha=1) + 
      #scale_fill_brewer(palette="Set3",name="Stock Group",breaks=mybreaks) + 
      scale_fill_manual(values=mycols,  name="Stock Group") +
      theme(axis.title.x = element_text(size=20),axis.text.x  = element_text(angle=45, size=15,color="black", hjust=1, vjust=1)) +
      theme(axis.title.y = element_text(size=20),axis.text.y  = element_text(angle=0, size=15,color="black")) +
      labs(title = labs_title) +
      theme(plot.title = element_text(size = rel(2))) +
      ylab("Catch (thousands)")+xlab("")+
      scale_x_continuous(expand=c(0,0),breaks=xtickseq)+
      scale_y_continuous(expand=c(0,0),breaks=tickseq)+
      theme(legend.title = element_text(size=15, face="bold"))+
      theme(legend.text = element_text(size = 15))+
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
      theme(panel.background = element_blank())+ #removes the gray filled back
      theme(panel.border = element_rect(fill=NA,linetype = "solid",colour = "black")) #Adds a border to plot 
#add caption (if asked)
	  if (caps_on==TRUE) {
	   xx<-as.character(sub1$FisheryNameLong)[1]
	   Caption<-paste("Appendix E",i,"  Chinook Model estimates of landed catch stock composition for ", xx," 1979-", max(sub1$Year), sep="")
      image <- arrangeGrob(image,sub=textGrob(Caption,x = 0, hjust = 0, vjust=0.1,
                                              gp = gpar( fontsize = 14)))
	  } else {
	     image <- arrangeGrob(image)
	  }
    plot(image)
}

#######
#ModObsCatFigure
######
#description
#This program is designed to create stacked area time series figures of Chinook-model derived
#stock composition of actual landed catch; the R code is fairly straightforward; however, the 
#prep of modeloutput as needed for the program requires additional work, spearheaded by Gayle in 2012
#NOTE: This is another somewhat painful use of ggplot, but it gets it done as needed...
#      ALSO, be sure to relabel as needed the APPENDIX LETTER AND YEAR associated with captions, 
#      which are written into image files here as a matter of convenience/function
#code from pete m, the original CTC R using badass
#inputs
#
#outputs

ModObsCatFigure <- function(x, fishnum = 1, caps_on = FALSE, minyear = NULL, maxyear = NULL, cat = "obs-mlc", labs_title = NULL) {
  #Load necessary packages
  suppressMessages(require(ggplot2))
  suppressMessages(require(gridExtra))
  suppressMessages(require(grid))
  suppressMessages(require(reshape2))
  #set fishery number to plot
  i = fishnum
  #determine max and min year (if not specified by user)
  if(is.null(minyear)) minyear = min(x$Year)
  if(is.null(maxyear)) maxyear = max(x$Year)
  #subset of data for each plot
  sub1<-subset(x,FisheryNum==i&Year<=maxyear&Year>=minyear)
  #determine title of figure (if not specified by user)
  if(is.null(labs_title)) labs_title = as.character(sub1$FisheryName[1])
  
  #determine while running each fishery what the axis tick breaks should be
  if(cat=="obs-mlc") { 
    lim<-max(sub1$Observed_Catch, sub1$Model_LCatch) 
    melt_vars = c("Year","Observed_Catch","Model_LCatch")
    }
  if(cat=="obs-mtm") { 
    lim<-max(sub1$Observed_Catch, sub1$Model_TM)
    melt_vars = c("Year","Observed_Catch","Model_TM")
   }
  if(cat=="mlc-mtm") { 
    lim<-max(sub1$Observed_Catch, sub1$Model_TM)
    melt_vars = c("Year","Model_LCatch","Model_TM")
  }
  if(cat=="all") { 
    lim<-max(sub1$Observed_Catch, sub1$Model_TM, sub1$Model_LCatch)
    melt_vars = c("Year","Observed_Catch","Model_LCatch","Model_TM")
    }
  if(lim <= 50000)
  {tickseq = seq(0,50,5)} else
    if(lim >= 1000000)
    {tickseq = round(seq(0,lim/1000,l=10),-2)} else
     if(lim >= 300000)
     {tickseq = seq(0,1000,50)} else
      if(lim >= 200000)
      {tickseq = seq(0,300,30)} else
        if(lim >100000)
        {tickseq = seq(0,200,20)} else
        {tickseq = seq(0,100,10)}

  #Convert data frame to 'long format' for easy plotting
  sub1_melt = melt(sub1[melt_vars], id=c("Year"))
  names(sub1_melt)[2] = "Catch_Type" #rename 'Variable' to "Catch Type'
  #find and replace all -999 with NA
  sub1_melt$value = ifelse(sub1_melt$value==-999,NA,sub1_melt$value)
  #create figure
  image<-ggplot(sub1_melt, aes(x=Year, y=value/1000, colour=Catch_Type)) + 
    geom_line() +
    theme(axis.title.x = element_text(size=20),axis.text.x  = element_text(angle=45, size=15,color="black", hjust=1, vjust=1)) +
    theme(axis.title.y = element_text(size=20),axis.text.y  = element_text(angle=0, size=15,color="black")) +
    labs(title = labs_title) +
    theme(plot.title = element_text(size = rel(2))) +
    ylab("Catch (thousands)")+xlab("")+
    scale_x_continuous(expand=c(0,0),breaks=seq(1980,maxyear,5))+
    scale_y_continuous(expand=c(0,0),breaks=tickseq)+
    theme(legend.title = element_text(size=15, face="bold"))+
    theme(legend.text = element_text(size = 15))+
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
    theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
    theme(panel.background = element_blank())+ #removes the gray filled back
    theme(panel.border = element_rect(fill=NA,linetype = "solid",colour = "black")) #Adds a border to plot 
  #add caption (if asked)
  if (caps_on==TRUE) {
    xx<-as.character(sub1$FisheryName)[1]
    Caption<-paste("Appendix ?",i,"  Chinook Model estimates of landed catch ", xx," 1979-", max(sub1$Year), sep="")
    image <- arrangeGrob(image,sub=textGrob(Caption,x = 0, hjust = 0, vjust=0.1,
                                            gp = gpar( fontsize = 14)))
  } else {
    image <- arrangeGrob(image)
  }
  plot(image)
}

#######
#StackedAreaTable
######
#description
#This program is designed to create stacked area time series figures of Chinook-model derived
#stock composition of actual landed catch; the R code is fairly straightforward; however, the 
#prep of modeloutput as needed for the program requires additional work, spearheaded by Gayle in 2012
#inputs
#
#outputs
#
#StackedAreaTable(x=CLB1804_stk_seakaddon, fishnum = 3, caps_on = FALSE, labs_title="CLB1804")

StackedAreaTable <- function(x, fishnum = 1, minyear = NULL, maxyear = NULL, gsicompare = FALSE, whichstocks = NULL) {
  #load data
  modcomp = x
  #set fishery number to plot
  i = fishnum
  #determine max and min year (if not specified by user)
  if(is.null(minyear)) minyear = min(modcomp$Year)
  if(is.null(maxyear)) maxyear = max(modcomp$Year)
  #subset of data for each plot
  sub1<-subset(modcomp,FisheryNum==i&Year<=maxyear&Year>=minyear)
  #set breaks to the current order (south-to-north)
  if(is.null(whichstocks)) {
    #mybreaks = c("CR-bright","CR-sp&su","CR-tule","FR-early","FR-late","GS","NCBC","ORCST","PSD","SEAK","WACST","WCVI") #old color order
    mybreaks = c('ORCST','CR-tule','CR-sp&su','CR-bright','WACST','PSD','FR-late','FR-early','GS','WCVI','NCBC','SEAK')
    #mybreaks = c('AK','Canada','FR-late','FR-early','SUS')
  } else {
    mybreaks = whichstocks
  }
  
  if(gsicompare) {
   #set breaks to the current order (south-to-north)
    if(is.null(whichstocks)) {
      #redefine 'breakpoints'
      mybreaks = c('ORCST','CR','WACST','PSD','FR-late','FR-early','GS','WCVI','NCBC','SEAK', 'NONE', 'UNKNOWN')
    } else {
      mybreaks = whichstocks
    }
    
    #if the 'none' category DOES not exist in the original data, add it!
    if(!("NONE" %in% levels(sub1$StockGroup))) { 
      tmp = subset(sub1, StockGroup==levels(sub1$StockGroup)[1])
      tmp$StockGroup = "NONE"
      tmp$PreferredCatch = 0
      tmp$PropinCatch = 0
      sub1 = rbind(sub1,tmp)
    }
    if(!("UNKNOWN" %in% levels(sub1$StockGroup))) { 
      tmp = subset(sub1, StockGroup==levels(sub1$StockGroup)[1])
      tmp$StockGroup = "UNKNOWN"
      tmp$PreferredCatch = 0
      tmp$PropinCatch = 0
      sub1 = rbind(sub1,tmp)
    }
  }
  sub1$StockGroup2 = factor(as.character(sub1$StockGroup), levels = mybreaks)
  
  out_est = with(sub1, tapply(PreferredCatch, list(factor(Year),StockGroup2), sum))
  out_est = as.data.frame(out_est)
  out_est$Total = rowSums(out_est, na.rm=TRUE)
  #if preferred catch var exists, i.e. is null = false
  if(!is.null(sub1$PreferredCatchVar)) {
    out_var = with(sub1, tapply(PreferredCatchVar, list(factor(Year),StockGroup2), sum))
    out_var = as.data.frame(out_var)
    out_var$Total = 0
  }
  #if preferred catch var does not exist, i.e. is null = true
  if(is.null(sub1$PreferredCatchVar)) {
    out_var = out_est
    for(i in 1:nrow(out_var)) out_var[i,]=0
  }
  out_formatted = out_est
  for(i in 1:nrow(out_est)) {
    for(j in 1:ncol(out_est)) {
      out_formatted[i,j] = paste(round(out_est[i,j],0), "(", round(sqrt(out_var[i,j]),0), ")", sep="")
      out_formatted[i,j] = paste(round(out_est[i,j]/1000,1), "(", round(sqrt(out_var[i,j])/1000,1), ")", sep="")
    }
  }
  out = list(est=out_est, var=out_var, sd=sqrt(out_var), formatted=out_formatted)
  return(out)
}

gsiscatterFUN <- function(gsi, mod, whichstocks = NULL) {

  if (is.null(whichstocks)) {
    mystklist = c('ORCST','CR','WACST','PSD','FR-late','FR-early','GS','WCVI','NCBC','SEAK')
  } else {
    mybreaks = whichstocks
  }
  
  ncomps = sum(mystklist %in% colnames(mod))
  par(mfrow=c(4,3), mar=c(3.1, 2.6, 2.6, 0.1)) 
  #ncomps = sum(colnames(gsi) %in% colnames(mod))
  #mystklist = colnames(gsi)
  for (i in 1:ncomps) {
    varname = mystklist[i]
    gsi_loc = grep(varname, colnames(gsi))
    mod_loc = grep(varname, colnames(mod))
    yvar = gsi[,gsi_loc]
    xvar = mod[,mod_loc]
    
    mycor= cor(yvar, xvar)
    mycor_formatted = paste("r=",round(mycor*100,1),"%",sep="")
    
    xylim = c(min(yvar,xvar), max(yvar,xvar))
    plot(yvar~xvar, ylab="", xlab="", ylim=xylim, xlim=xylim, pch=19, bty="l", xaxt="n", yaxt="n")
    abline(a=0,b=1)
    mtext(varname, side=3, adj=0, line=1, cex=1.5)
    mtext("GSI", side=2, line=.5)
    mtext("Model", side=1, line=.5)
    mtext(mycor_formatted, side=3, adj=1, line=0, cex=.8)
  }
  #how to interpret a figure, figure
  plot(rbind(c(1,1),c(100,100)),col="white",bty="l",xaxt="n", yaxt="n",ylab="",xlab="")
  mtext("HOW TO INTERPRET", side=3, adj=0, line=1, cex=1.5)
  mtext("GSI", side=2, line=.5)
  mtext("Model", side=1, line=.5)
  text(0,95, "Model", cex=1.25, adj=0, col="blue")
  text(0,80, "Underpredicts", cex=1.25, adj=0, col="blue")
  abline(a=0,b=1)
  text(100,20, "Model", cex=1.25, adj=1, col="red")
  text(100,5, "Overpredicts", cex=1.25, adj=1, col="red")
}


gsilineFUN <- function(gsi, mod, ci_alpha=0.05, whichstocks = c("ORCST","CR","WACST","PSD","FR-late","FR-early","GS","WCVI","NCBC","SEAK")) {
  #compute 95% CI's
  gsi$ub = gsi$est + gsi$sd*qnorm((1-ci_alpha/2),0,1)
  gsi$lb = gsi$est - gsi$sd*qnorm((1-ci_alpha/2),0,1)
  #convert everything to catch in 1000's
  gsi$est=gsi$est/1000
  gsi$ub=gsi$ub/1000
  gsi$lb=gsi$lb/1000
  mod$est=mod$est/1000
  #compute number of stocks
  nstocks = length(whichstocks)
  if(!all(whichstocks %in% names(gsi$est))) cat("WARNING in gsilineFUN: stocks specified in 'whichstocks' does not match stocks in the provided data\n")
  par(mfrow=c(4,3), mar=c(3.1, 4.6, 2.6, 0.1)) 
  #for each
  for(i in 1:nstocks) {
    stockloc = grep(whichstocks[i],names(gsi$est))
    YEAR = as.numeric(rownames(gsi$est))
    ymax = max(gsi$est[,i],gsi$lb[,i],gsi$ub[,i],mod$est[,i])
    mycor= cor(gsi$est[,i], mod$est[,i])
    mycor_formatted = paste("r=",round(mycor*100,1),"%",sep="")
    if(i==1) ymax = ymax/0.6 #inflate by 40% to increase the 'room' on the plot
    ymin = max(0,min(gsi$est[,i],gsi$lb[,i],gsi$ub[,i],mod$est[,i])) #either truncate at 0, or set to the min
    plot(YEAR,gsi$est[,i], lwd=1.25, ylab="Catch (thousands)", xlab="Accounting Year", ylim=c(ymin,ymax), pch=19, bty="l", cex.main=1.5)
    mtext(mycor_formatted, side=3, adj=1, line=0, cex=.8)
    mtext(whichstocks[i], side=3, adj=0, line=1, cex=1.5)
    for(j in 1:length(YEAR)) lines(cbind(c(YEAR[j],YEAR[j]),c(gsi$ub[j,i],gsi$lb[j,i])))
    points(YEAR,mod$est[,i], lwd=1.25, col="red", pch=4)
    if(i==1) legend("topright", legend=c("GSI", "Chinook Model"), col=c("black","red"), pch=c(19, 4), bty="n")
  }
}

compareGSItableFUN <- function(x, y, q, w, ci_alpha=0.05, whichstocks=c("ORCST","CR","WACST","PSD","FR-late","FR-early","GS","WCVI","NCBC","SEAK"), whichcompare="rollup") {
  if(whichcompare=="rollup") { 
    #landed catch calculations
    CM_CLB1804=colSums(x$est[whichstocks])/1000
    GSI_CLB1804=colSums(y$est[whichstocks], na.rm=TRUE)/1000
    DIFF_CLB1804=GSI_CLB1804-CM_CLB1804
    DIFF_CLB1804_SD=sqrt(colSums(y$var[whichstocks])/1000^2)
    CM_Phase2=colSums(q$est[whichstocks])/1000
    GSI_Phase2=colSums(w$est[whichstocks], na.rm=TRUE)/1000
    DIFF_Phase2=GSI_Phase2-CM_Phase2    
    DIFF_Phase2_SD=sqrt(colSums(w$var[whichstocks])/1000^2)
    Compare_CLB1804 = CM_CLB1804
    Compare_CLB1804 = ""
    Compare_Phase2 = CM_CLB1804
    Compare_Phase2 = ""
    myData_Table_Catch = rbind(Compare_CLB1804, CM_CLB1804=round(CM_CLB1804,1), GSI_CLB1804=round(GSI_CLB1804,1), SD=paste("(",round(DIFF_CLB1804_SD,1),")",sep=""), error=round(GSI_CLB1804-CM_CLB1804,1), Compare_Phase2, CM_Phase2=round(CM_Phase2,1), GSI_Phase2=round(GSI_Phase2,1), SD=paste("(",round(DIFF_Phase2_SD,1),")",sep=""), error=round(GSI_Phase2-CM_Phase2,1))
    
    mse_CLB1804 = sum(DIFF_CLB1804^2)/length(whichstocks)
    rmse_CLB1804 = sqrt(mse_CLB1804)
    mpe_CLB1804 = sum((GSI_CLB1804-CM_CLB1804)/GSI_CLB1804)/length(whichstocks) * 100
    mape_CLB1804 = sum(abs((GSI_CLB1804-CM_CLB1804)/GSI_CLB1804))/length(whichstocks) * 100
    mse_phase2 = sum(DIFF_Phase2^2)/length(DIFF_Phase2)
    rmse_phase2 = sqrt(mse_phase2)
    mpe_phase2 = sum((GSI_Phase2-CM_Phase2)/GSI_Phase2)/length(whichstocks) * 100
    mape_phase2 = sum(abs((GSI_Phase2-CM_Phase2)/GSI_Phase2))/length(whichstocks) * 100
    myData_Error_Catch = data.frame(MSE=c(mse_CLB1804,mse_phase2), RMSE=c(rmse_CLB1804,rmse_phase2), MPE=c(mpe_CLB1804,mpe_phase2), MAPE=c(mape_CLB1804,mape_phase2))
    rownames(myData_Error_Catch) = c("CLB1804", "Phase 2")

    #proportion calculations
    CM_CLB1804=colSums(x$est[whichstocks])/sum(x$est[whichstocks])
    GSI_CLB1804=colSums(y$est[whichstocks], na.rm=TRUE)/sum(y$est$Total, na.rm=TRUE)
    DIFF_CLB1804=GSI_CLB1804-CM_CLB1804    
    DIFF_CLB1804_SD=sqrt(colSums(y$var[whichstocks])*(1/sum(y$est$Total)^2))
    CM_Phase2=colSums(q$est[whichstocks])/sum(q$est$Total)
    GSI_Phase2=colSums(w$est[whichstocks], na.rm=TRUE)/sum(w$est$Total, na.rm=TRUE)
    DIFF_Phase2=GSI_Phase2-CM_Phase2    
    DIFF_Phase2_SD=sqrt(colSums(w$var[whichstocks])*(1/sum(w$est$Total)^2))
    Compare_CLB1804 = CM_CLB1804
    Compare_CLB1804 = ""
    Compare_Phase2 = CM_CLB1804
    Compare_Phase2 = ""
    myData_Table_Prop = rbind(Compare_CLB1804, CM_CLB1804=round(CM_CLB1804,2), GSI_CLB1804=round(GSI_CLB1804,2), SD=paste("(",round(DIFF_CLB1804_SD,3),")",sep=""), error=round(GSI_CLB1804-CM_CLB1804,2), Compare_Phase2, CM_Phase2=round(CM_Phase2,2), GSI_Phase2=round(GSI_Phase2,2), SD=paste("(",round(DIFF_Phase2_SD,3),")",sep=""), error=round(GSI_Phase2-CM_Phase2,2))
    
    mse_CLB1804 = sum(DIFF_CLB1804^2)/length(whichstocks)
    rmse_CLB1804 = sqrt(mse_CLB1804)
    mpe_CLB1804 = sum((GSI_CLB1804-CM_CLB1804)/GSI_CLB1804)/length(whichstocks) * 100
    mape_CLB1804 = sum(abs((GSI_CLB1804-CM_CLB1804)/GSI_CLB1804))/length(whichstocks) * 100
    mse_phase2 = sum(DIFF_Phase2^2)/length(whichstocks)
    rmse_phase2 = sqrt(mse_phase2)
    mpe_phase2 = sum((GSI_Phase2-CM_Phase2)/GSI_Phase2)/length(whichstocks) * 100
    mape_phase2 = sum(abs((GSI_Phase2-CM_Phase2)/GSI_Phase2))/length(whichstocks)* 100
    myData_Error_Prop = data.frame(MSE=c(mse_CLB1804,mse_phase2), RMSE=c(rmse_CLB1804,rmse_phase2), MPE=c(mpe_CLB1804,mpe_phase2), MAPE=c(mape_CLB1804,mape_phase2))
    rownames(myData_Error_Prop) = c("CLB1804", "Phase 2")

    blankrow = ""
    tabletype = "portrait_catch"
    if(tabletype=="landscape_catchCOMBINED") {
      #old format...
      myData_Error_Combined = as.data.frame(rbind(blankrow, round(myData_Error_Catch,1), blankrow, round(myData_Error_Prop,2)))
      rownames(myData_Error_Combined)[c(1,4,5:6)] = c("Landed Catch", "Proportion", "CLB1804 ","Phase 2 ")      
    }
    
    if(tabletype=="landscape_catch") {
      #NOTE DOES NOT INCLUDE PROPORTION!
      myData_Error_Combined = as.data.frame(rbind(round(myData_Error_Catch,1)))
    }
    
    if(tabletype=="portrait_catch") {
      #NOTE DOES NOT INCLUDE PROPORTION!
      myData_Error_Combined = t(cbind(round(myData_Error_Catch[,1],1), round(myData_Error_Catch[,2],1), round(myData_Error_Catch[,3],1), round(myData_Error_Catch[,4],1)))
      colnames(myData_Error_Combined) = c("CLB1804","Phase 2")
      rownames(myData_Error_Combined) = c("MSE","RMSE","MPE","MAPE")      
    }    

    } #end 'rollup' error calcs

  if(whichcompare=="annual") {

    percerror <- function(gsi, cm) {
      out=matrix(NA,nrow=nrow(gsi),ncol=ncol(gsi))
      for(i in 1:nrow(gsi)) {
        for(j in 1:ncol(gsi)) {
          out[i,j]=ifelse(gsi[i,j]==0,0,(gsi[i,j]-cm[i,j])/gsi[i,j])
        }
      }
      return(out)
    }
    
    #landed catch calculations
    CM_CLB1804=x$est[whichstocks]/1000
    GSI_CLB1804=y$est[whichstocks]/1000
    DIFF_CLB1804=GSI_CLB1804-CM_CLB1804
    DIFF_CLB1804_SD=sqrt(y$var[whichstocks])/1000
    CM_Phase2=q$est[whichstocks]/1000
    GSI_Phase2=w$est[whichstocks]/1000
    DIFF_Phase2=GSI_Phase2-CM_Phase2    
    DIFF_Phase2_SD=sqrt(w$var[whichstocks])/1000
    Compare_CLB1804 = CM_CLB1804
    Compare_CLB1804 = ""
    Compare_Phase2 = CM_CLB1804
    Compare_Phase2 = ""
    myData_Table_Catch = rbind(Compare_CLB1804, CM_CLB1804=round(CM_CLB1804,1), GSI_CLB1804=round(GSI_CLB1804,1), SD=round(DIFF_CLB1804_SD,1), error=round(GSI_CLB1804-CM_CLB1804,1), Compare_Phase2, CM_Phase2=round(CM_Phase2,1), GSI_Phase2=round(GSI_Phase2,1), SD=round(DIFF_Phase2_SD,1), error=round(GSI_Phase2-CM_Phase2,1))

    mse_CLB1804 = colSums(DIFF_CLB1804^2)/nrow(DIFF_CLB1804)
    rmse_CLB1804 = sqrt(mse_CLB1804)
    mpe_CLB1804 = colSums(percerror(GSI_CLB1804,CM_CLB1804))/nrow(DIFF_CLB1804) * 100
    mape_CLB1804 = colSums(abs(percerror(GSI_CLB1804,CM_CLB1804)))/nrow(DIFF_CLB1804) * 100
    mse_phase2 = colSums(DIFF_Phase2^2)/nrow(DIFF_Phase2)
    rmse_phase2 = sqrt(mse_phase2)
    mpe_phase2 = colSums(percerror(GSI_Phase2,CM_Phase2))/nrow(DIFF_Phase2) * 100
    mape_phase2 = colSums(abs(percerror(GSI_Phase2,CM_Phase2)))/nrow(DIFF_Phase2) * 100
    myData_Error_Catch = data.frame(ChinookModel=c(rep("CLB1804",length(whichstocks)),rep("Phase 2",length(whichstocks))),StockGroup=c(rep(whichstocks,2)),MSE=c(mse_CLB1804,mse_phase2), RMSE=c(rmse_CLB1804,rmse_phase2), MPE=c(mpe_CLB1804,mpe_phase2), MAPE=c(mape_CLB1804,mape_phase2))
    myData_Error_Catch$ChinookModel = as.character(myData_Error_Catch$ChinookModel)
    myData_Error_Catch$StockGroup = as.character(myData_Error_Catch$StockGroup)
    
    #proportion calculations
    CM_CLB1804=x$est[whichstocks]/x$est$Total #proportions are calculated for ALL stocks, not just stocks included in the comparison
    GSI_CLB1804=y$est[whichstocks]/y$est$Total
    DIFF_CLB1804=GSI_CLB1804-CM_CLB1804    
    DIFF_CLB1804_SD=sqrt(y$var[whichstocks]*(1/y$est$Total^2))
    CM_Phase2=q$est[whichstocks]/q$est$Total
    GSI_Phase2=w$est[whichstocks]/w$est$Total
    DIFF_Phase2=GSI_Phase2-CM_Phase2    
    DIFF_Phase2_SD=sqrt(w$var[whichstocks]*(1/w$est$Total^2))
    Compare_CLB1804 = CM_CLB1804
    Compare_CLB1804 = ""
    Compare_Phase2 = CM_CLB1804
    Compare_Phase2 = ""
    myData_Table_Prop = rbind(Compare_CLB1804, CM_CLB1804=round(CM_CLB1804,2), GSI_CLB1804=round(GSI_CLB1804,2), SD=round(DIFF_CLB1804_SD,3), error=round(GSI_CLB1804-CM_CLB1804,2), Compare_Phase2, CM_Phase2=round(CM_Phase2,2), GSI_Phase2=round(GSI_Phase2,2), SD=round(DIFF_Phase2_SD,3), error=round(GSI_Phase2-CM_Phase2,2))
 
    mse_CLB1804 = colSums(DIFF_CLB1804^2)/nrow(DIFF_CLB1804)
    rmse_CLB1804 = sqrt(mse_CLB1804)
    mpe_CLB1804 = colSums((GSI_CLB1804-CM_CLB1804)/GSI_CLB1804)/nrow(DIFF_CLB1804) * 100
    mape_CLB1804 = colSums(abs((GSI_CLB1804-CM_CLB1804)/GSI_CLB1804))/nrow(DIFF_CLB1804) * 100
    mse_phase2 = colSums(DIFF_Phase2^2)/nrow(DIFF_Phase2)
    rmse_phase2 = sqrt(mse_phase2)
    mpe_phase2 = colSums((GSI_Phase2-CM_Phase2)/GSI_Phase2)/nrow(DIFF_Phase2) * 100
    mape_phase2 = colSums(abs((GSI_Phase2-CM_Phase2)/GSI_Phase2))/nrow(DIFF_Phase2) * 100
    myData_Error_Prop = data.frame(ChinookModel=c(rep("CLB1804",length(whichstocks)),rep("Phase 2",length(whichstocks))),StockGroup=c(rep(whichstocks,2)),MSE=c(mse_CLB1804,mse_phase2), RMSE=c(rmse_CLB1804,rmse_phase2), MPE=c(mpe_CLB1804,mpe_phase2), MAPE=c(mape_CLB1804,mape_phase2))
    myData_Error_Prop$ChinookModel = as.character(myData_Error_Prop$ChinookModel)
    myData_Error_Prop$StockGroup = as.character(myData_Error_Prop$StockGroup)
    

    blankrow = ""
    tabletype = "portrait_catch"
 
    if (tabletype=="landscape_catch") {
      myData_Error_Combined = cbind(subset(myData_Error_Catch,ChinookModel=="CLB1804")[,2], 
                                                              round(cbind(subset(myData_Error_Catch,ChinookModel=="CLB1804")$MSE,
                                                                          subset(myData_Error_Catch,ChinookModel=="Phase 2")$MSE,
                                                                          subset(myData_Error_Catch,ChinookModel=="CLB1804")$RMSE,
                                                                          subset(myData_Error_Catch,ChinookModel=="Phase 2")$RMSE,
                                                                          subset(myData_Error_Catch,ChinookModel=="CLB1804")$MPE,
                                                                          subset(myData_Error_Catch,ChinookModel=="Phase 2")$MPE,
                                                                          subset(myData_Error_Catch,ChinookModel=="CLB1804")$MAPE,
                                                                          subset(myData_Error_Catch,ChinookModel=="Phase 2")$MAPE
                                                              ),1))
      colnames(myData_Error_Combined) = c("StockGroup", "MSECLB1804", "MSEPhase 2", "RMSECLB1804", "RMSEPhase 2", "MPECLB1804", "MPEPhase 2", "MAPECLB1804", "MAPEPhase 2")
    }
    
    if (tabletype=="portrait_catch") {
      myData_Error_Combined = rbind(         blankrow,round(subset(myData_Error_Catch,ChinookModel=="CLB1804")$MSE,1),
                                                      round(subset(myData_Error_Catch,ChinookModel=="Phase 2")$MSE,1),
                                             blankrow,round(subset(myData_Error_Catch,ChinookModel=="CLB1804")$RMSE,1),
                                                      round(subset(myData_Error_Catch,ChinookModel=="Phase 2")$RMSE,1),
                                             blankrow,round(subset(myData_Error_Catch,ChinookModel=="CLB1804")$MPE,1),
                                                      round(subset(myData_Error_Catch,ChinookModel=="Phase 2")$MPE,1),
                                             blankrow,round(subset(myData_Error_Catch,ChinookModel=="CLB1804")$MAPE,1),
                                                      round(subset(myData_Error_Catch,ChinookModel=="Phase 2")$MAPE,1)
                                          )
      colnames(myData_Error_Combined) = subset(myData_Error_Catch,ChinookModel=="CLB1804")[,2]
      rownames(myData_Error_Combined) = c("MSE", "CLB1804", "Phase 2", "RMSE", "CLB1804", "Phase 2", "MPE", "CLB1804", "Phase 2", "MAPE", "CLB1804", "Phase 2")
    }
    
    
    
  if (tabletype=="landscape") {
      myData_Error_Combined = rbind(blankrow1=blankrow, cbind(subset(myData_Error_Catch,ChinookModel=="CLB1804")[,2], 
                                                              round(cbind(subset(myData_Error_Catch,ChinookModel=="CLB1804")$MSE,
                                                                          subset(myData_Error_Catch,ChinookModel=="Phase 2")$MSE,
                                                                          subset(myData_Error_Catch,ChinookModel=="CLB1804")$RMSE,
                                                                          subset(myData_Error_Catch,ChinookModel=="Phase 2")$RMSE,
                                                                          subset(myData_Error_Catch,ChinookModel=="CLB1804")$MPE,
                                                                          subset(myData_Error_Catch,ChinookModel=="Phase 2")$MPE,
                                                                          subset(myData_Error_Catch,ChinookModel=="CLB1804")$MAPE,
                                                                          subset(myData_Error_Catch,ChinookModel=="Phase 2")$MAPE
                                                              ),2)), 
                                              blankrow2=blankrow, cbind(subset(myData_Error_Prop,ChinookModel=="CLB1804")[,2], 
                                                              round(cbind(subset(myData_Error_Prop,ChinookModel=="CLB1804")$MSE,
                                                                          subset(myData_Error_Prop,ChinookModel=="Phase 2")$MSE,
                                                                          subset(myData_Error_Prop,ChinookModel=="CLB1804")$RMSE,
                                                                          subset(myData_Error_Prop,ChinookModel=="Phase 2")$RMSE,
                                                                          subset(myData_Error_Prop,ChinookModel=="CLB1804")$MPE,
                                                                          subset(myData_Error_Prop,ChinookModel=="Phase 2")$MPE,
                                                                          subset(myData_Error_Prop,ChinookModel=="CLB1804")$MAPE,
                                                                          subset(myData_Error_Prop,ChinookModel=="Phase 2")$MAPE
                                                              ),2))
      )
    colnames(myData_Error_Combined) = c("StockGroup", "MSECLB1804", "MSEPhase 2", "RMSECLB1804", "RMSEPhase 2", "MPECLB1804", "MPEPhase 2", "MAPECLB1804", "MAPEPhase 2")
    myData_Error_Combined[c(1,length(whichstocks)+2),1] = c("Landed Catch", "Proportion")
  }

    if (tabletype=="portrait") { 
      myData_Error_Combined = as.data.frame(rbind(blankrow, cbind(myData_Error_Catch[,1:2], round(myData_Error_Catch[,3:6],1)), 
                                                  blankrow, cbind(myData_Error_Prop[,1:2],round(myData_Error_Prop[,3:6],2))))     
      myData_Error_Combined$ChinookModel[c(1,nrow(myData_Error_Catch)+2)] = c("Landed Catch", "Proportion")
      }

  } #end 'annual' error calcs
  
    
  out = list(DataTableFormat_Catch=myData_Table_Catch, DataTableFormat_Prop=myData_Table_Prop, ErrorTableFormat_Catch=myData_Error_Catch, ErrorTableFormat_Prop=myData_Error_Prop, ErrorTableFormat_Combined=myData_Error_Combined)
  return(out)
}

compareGSIfigureFUN <- function(x, y, q, w, ci_alpha=0.05, whichstocks=c("ORCST","CR","WACST","PSD","FR-late","FR-early","GS","WCVI","NCBC","SEAK"), whichcompare=c("catch","prop"), plotcompare=TRUE) {
  suppressMessages(require(ggplot2))
  if(whichcompare=="catch") {
    CM_CLB1804=colSums(x$est[whichstocks])/1000
    GSI_CLB1804=colSums(y$est[whichstocks], na.rm=TRUE)/1000
    DIFF_CLB1804=GSI_CLB1804-CM_CLB1804
    DIFF_CLB1804_SD = sqrt(colSums(y$var[whichstocks])/1000^2)
    CM_Phase2=colSums(q$est[whichstocks])/1000
    GSI_Phase2=colSums(w$est[whichstocks], na.rm=TRUE)/1000
    DIFF_Phase2_SD = sqrt(colSums(w$var[whichstocks])/1000^2)
    Compare_CLB1804 = CM_CLB1804
    Compare_CLB1804 = ""
    Compare_Phase2 = CM_CLB1804
    Compare_Phase2 = ""
    myData_Table = rbind(Compare_CLB1804, CM_CLB1804, GSI_CLB1804, SD=paste("(",round(DIFF_CLB1804_SD,1),")",sep=""), GSI_CLB1804-CM_CLB1804, Compare_Phase2, CM_Phase2, GSI_Phase2, SD=paste("(",round(DIFF_Phase2_SD,1),")",sep=""), GSI_Phase2-CM_Phase2)
  }
  if(whichcompare=="prop") {
    CM_CLB1804=colSums(x$est[whichstocks])/sum(x$est[whichstocks])
    GSI_CLB1804=colSums(y$est[whichstocks], na.rm=TRUE)/sum(y$est$Total, na.rm=TRUE)
    DIFF_CLB1804_SD=sqrt(colSums(y$var[whichstocks])*(1/sum(y$est$Total)^2)) #note 'Total' must be used, which WILL include harvest in the 'unk' category (or any other stock groups)
    CM_Phase2=colSums(q$est[whichstocks])/sum(q$est[whichstocks])
    GSI_Phase2=colSums(w$est[whichstocks], na.rm=TRUE)/sum(w$est$Total, na.rm=TRUE)
    DIFF_Phase2_SD=sqrt(colSums(w$var[whichstocks])*(1/sum(w$est$Total)^2))  
    Compare_CLB1804 = CM_CLB1804
    Compare_CLB1804 = ""
    Compare_Phase2 = CM_CLB1804
    Compare_Phase2 = ""
    myData_Table = rbind(Compare_CLB1804, CM_CLB1804, GSI_CLB1804, SD=paste("(",round(DIFF_CLB1804_SD,3),")",sep=""), GSI_CLB1804-CM_CLB1804, Compare_Phase2, CM_Phase2, GSI_Phase2, SD=paste("(",round(DIFF_Phase2_SD,3),")",sep=""), GSI_Phase2-CM_Phase2)
  }
  
  ylabel = ifelse(whichcompare=="catch","Catch (thousands)","Proportion")  
  
  MOD1_myData_Figure = data.frame(StockGroup=c(names(CM_CLB1804), names(GSI_CLB1804)), DataSource=c(rep("Chinook Model",length(CM_CLB1804)),rep("GSI",length(GSI_CLB1804))), Value=c(CM_CLB1804,GSI_CLB1804), SD=c(rep(NA,length(CM_CLB1804)),DIFF_CLB1804_SD))
  MOD1_limits <- aes(ymax = MOD1_myData_Figure$Value + MOD1_myData_Figure$SD*qnorm((1-ci_alpha/2),0,1),
                     ymin = MOD1_myData_Figure$Value - MOD1_myData_Figure$SD*qnorm((1-ci_alpha/2),0,1))

  MOD2_myData_Figure = data.frame(StockGroup=c(names(CM_Phase2), names(GSI_Phase2)), DataSource=c(rep("Chinook Model",length(CM_Phase2)),rep("GSI",length(GSI_Phase2))), Value=c(CM_Phase2,GSI_Phase2), SD=c(rep(NA,length(CM_Phase2)),DIFF_Phase2_SD))
  MOD2_limits <- aes(ymax = MOD2_myData_Figure$Value + MOD2_myData_Figure$SD*qnorm((1-ci_alpha/2),0,1),
                     ymin = MOD2_myData_Figure$Value - MOD2_myData_Figure$SD*qnorm((1-ci_alpha/2),0,1))
  
  if(plotcompare==TRUE) {
    MOD1_p1 <- ggplot(data = MOD1_myData_Figure, aes(x = StockGroup, y = Value,fill = DataSource)) +
      geom_bar(stat = "identity",position = position_dodge(0.9), colour="black") + #barplot
      scale_fill_brewer(palette="Set1")+
      geom_errorbar(MOD1_limits, position = position_dodge(0.9),width = 0.25) + #error bars in barplot
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
      theme(panel.background = element_blank())+ #removes the gray filled back
      theme(axis.ticks.x=element_blank())+
      labs(x = "Stock Group", y = ylabel) +
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1.5, size=12))+
      theme(axis.text.y = element_text(size=12))+
      geom_hline(yintercept=0,color="black")+
      ggtitle("CLB1804 Comparison")
    
    MOD2_p1 <- ggplot(data = MOD2_myData_Figure, aes(x = StockGroup, y = Value,fill = DataSource)) +
      geom_bar(stat = "identity",position = position_dodge(0.9), colour="black") + #barplot
      scale_fill_brewer(palette="Set1")+
      geom_errorbar(MOD2_limits, position = position_dodge(0.9),width = 0.25) + #error bars in barplot
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
      theme(panel.background = element_blank())+ #removes the gray filled back
      theme(axis.ticks.x=element_blank())+
      labs(x = "Stock Group", y = ylabel) +
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1.5, size=12))+
      theme(axis.text.y = element_text(size=12))+
      geom_hline(yintercept=0,color="black")+
      ggtitle("CLB1804 Comparison")

    MOD1_p2 <- ggplot(data = MOD1_myData_Figure, aes(x = StockGroup, y = Value, color = DataSource)) +
      geom_point(aes(shape=DataSource))+
      scale_shape_manual(values=c(4,19))+
      scale_size_manual(values=c(5,1))+
      geom_errorbar(MOD1_limits, width=0)+#error bars in barplot
      theme_classic()+
      labs(x = "Stock Group", y = ylabel) +
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=12))+
      theme(axis.text.y = element_text(size=12))+
      scale_color_manual(values=c('red','blue'))+
      ggtitle("CLB1804 Comparison")
    
    MOD2_p2 <- ggplot(data = MOD2_myData_Figure, aes(x = StockGroup, y = Value, color = DataSource)) +
      geom_point(aes(shape=DataSource))+
      scale_shape_manual(values=c(4,19))+
      scale_size_manual(values=c(5,1))+
      geom_errorbar(MOD2_limits, width=0)+#error bars in barplot
      theme_classic()+
      labs(x = "Stock Group", y = ylabel)+
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, size=12))+
      theme(axis.text.y = element_text(size=12))+
      scale_color_manual(values=c('red','blue'))+
      ggtitle("Phase 2 Comparison")
  }
  
  out = list(DataTableFormat=myData_Table, DataFigureFormat_Model1=MOD1_myData_Figure, DataFigureFormat_Model2=MOD2_myData_Figure, Figure1_Model1=MOD1_p1, Figure1_Model2=MOD2_p1, Figure2_Model1=MOD1_p2, Figure2_Model2=MOD2_p2)
  return(out)
}
