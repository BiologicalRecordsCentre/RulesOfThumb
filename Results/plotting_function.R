# Calculate which datasets are likely to produce good or bad models
calc_bad <- function(df){
  df$Node1 <- df$P90>=3.4
  df$Node2 <- df$P90>=8.65
  df$Node3 <- df$prop_abs>=0.9867
  df$data_good <- df$data_aspire <- rep('bad',length(df$species))
  df$data_good[(df$Node1)&(df$Node2|df$Node3)] <- 'good'
  df$data_aspire[df$Node2] <- 'good'
  df
}

# Function to extract data from data frame
#  df = a trendsData data frame
#  prop = is the graph to show proportion of species (vs absolute numbers)
#  hab = habitat of interest
#  reg = region of interest, either full name or NUTS code
#  zeroes = are species with no records (Spnvisits==0) to be included?
#  aspire = is the cutoff the aspirational target (10:1 good:bad)?
#  melt = is the long table for plotting required?
num_spec <- function(df,hab=NULL,reg=NULL,prop=FALSE,melt=TRUE,
                     zeroes=TRUE,aspire=FALSE){
  taxa_num <- NULL
  if(!zeroes){
    df <- df[df$Spnvisits!=0,]
  }
  for(taxa in sort(unique(df$Taxa))){
    numrec <- length(unique(df$species[df$Taxa==taxa]))
    taxa_num <- rbind(taxa_num,
                      data.frame(taxa = taxa,
                                 numrec = numrec))
  }
  
  if(!is.null(hab)){
    df <- df[as.character(df$habitat)==hab,]
  }
  if(!is.null(reg)){
    if(grepl('UK[A-Z]',reg)){
      df <- df[as.character(df$code)==reg,]
    } else {
      df <- df[as.character(df$region)==reg,]
    }
  }
  taxa_count <- NULL
  for(taxa in sort(unique(taxa_num$taxa))){
    if(aspire){
      data_good <- length(df$Taxa[df$Taxa==taxa&df$data_aspire=='good'])
      data_bad  <- length(df$Taxa[df$Taxa==taxa&df$data_aspire=='bad'])
    } else {
      data_good <- length(df$Taxa[df$Taxa==taxa&df$data_good=='good'])
      data_bad  <- length(df$Taxa[df$Taxa==taxa&df$data_good=='bad'])
    }
    no_data <- length(df$Taxa[df$Taxa==taxa&df$Spnvisits==0])
    tmpdf <- data.frame(taxa = taxa,
                        no_data = no_data,
                        bad = data_bad - no_data,
                        good = data_good)
    taxa_count <- rbind(taxa_count,tmpdf)
  }
  
  taxa_count <- merge(taxa_count,taxa_num)
  taxa_count$no_data <- taxa_count$numrec - taxa_count$bad - taxa_count$good
  
  # Convert data table for turning into graphs
  taxa_melt <- melt(taxa_count, id=c('numrec','taxa'))
  taxa_prop <- taxa_count
  taxa_prop$bad <- taxa_prop$bad/taxa_prop$numrec
  taxa_prop$good <- taxa_prop$good/taxa_prop$numrec
  taxa_prop$no_data <- taxa_prop$no_data/taxa_prop$numrec
  taxa_prop$no_data[taxa_prop$numrec==0] <- 1
  taxa_prop$bad[taxa_prop$numrec==0] <- 0
  taxa_prop$good[taxa_prop$numrec==0] <- 0
  taxa_prop_melt <- melt(taxa_prop, id=c('numrec','taxa'))
  if(prop){
    taxa_melt <- taxa_prop_melt
  }
  if(melt){
    return(taxa_melt)
  } else {
    return(taxa_count)
  }
}
# Function to plot data extracted from data frame from num_spec function
#  df = a data frame from num_spec
#  prefix = a prefix to the title to describe the area or habitat of interest
stack_taxa <- function(df,prefix=NULL){
  if('no_data' %in% as.character(unique(df$variable))){
    colours = c('#CCCCCC','red', '#9999FF')
  } else {
    colours = c('red', '#9999FF')
  }
  if(max(df$value)<=1){
    ylabel <- 'Proportion of Species'
  } else {
    ylabel <- 'Number of Species'
  }
  if(!is.null(prefix)){
    title <- paste0(prefix,': ',ylabel,' which can be modelled')
  } else {
    title <- paste(ylabel,'which can be modelled')
  }
  ggplot(data=df,
         aes(fill=variable,y=value,x=taxa)) +
    geom_bar(stat='identity',alpha = .5) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust = 0.3)) +
    labs(fill = 'Data',x='Taxonomic Group',y=ylabel) +
    scale_fill_manual(values = colours) +
    ggtitle(title)
}
