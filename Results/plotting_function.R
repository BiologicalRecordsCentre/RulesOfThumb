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
num_spec <- function(df,hab=NULL,reg=NULL,prop=FALSE,
                     zeroes=TRUE,aspire=FALSE){
  if(!zeroes){
    df <- df[df$Spnvisits!=0,]
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
  taxa_count <- data.frame(matrix(ncol = 5, nrow = 0))
  for(taxa in sort(unique(df$Taxa))){
    numrec <- length(df$Taxa[df$Taxa==taxa])
    num0 <- length(df$Taxa[df$Taxa==taxa&df$Spnvisits==0])
    if(aspire){
      data_good <- length(df$Taxa[df$Taxa==taxa&df$data_aspire=='good'])
    } else {
      data_good <- length(df$Taxa[df$Taxa==taxa&df$data_good=='good'])
    }
    tmpdf <- data.frame(taxa = taxa,
                        numrec = numrec,
                        no_data = num0,
                        bad = numrec - data_good - num0,
                        good = data_good)
    taxa_count <- rbind(taxa_count,tmpdf)
  }
  
  # Convert data table for turning into graphs
  taxa_melt <- melt(taxa_count, id=c('numrec','taxa'))
  taxa_prop <- taxa_count
  taxa_prop$bad <- taxa_prop$bad/taxa_prop$numrec
  taxa_prop$good <- taxa_prop$good/taxa_prop$numrec
  taxa_prop$no_data <- taxa_prop$no_data/taxa_prop$numrec
  taxa_prop_melt <- melt(taxa_prop, id=c('numrec','taxa'))
  if(prop){
    taxa_melt <- taxa_prop_melt
  }
  taxa_melt
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
  if(max(df$value<=1)){
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