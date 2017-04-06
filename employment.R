#load the package
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("data.table","plyr","dplyr","ggplot2","stringr","maps",
              "bit64","RColorBrewer","choroplethr","gdata","sqldf")
ipak(packages)

#set working directory
setwd("/Users/jingan/Dropbox/project/chapter5")
setwd("/home/jingan/Dropbox/Github/PracticeDataScience/chapter5")

#load the data
ann2015 <- fread('2015.annual.singlefile.csv')

sqldf('attach blsdb as new')
read.csv.sql('2015.annual.singlefile.csv',
             sql='create table main.ann2015 as select * from file',
             dbname='blsdb')
ann<- sqldf("select * from main.ann2015", dbname='blsdb')

ann2015 <- fread('2015.annual.singlefile.csv', sep=',',
                 colClasses=c('character', 'integer', 'integer', 'integer',
                              'integer', 'integer', 'character',rep('integer',31)))


ann2015 <- ann2015[,1:15,with = FALSE]

for(u in c('agglevel','area','industry',
           'ownership','size')){
  assign(u,read.csv(paste(u,'_titles.csv',sep=''),
                    stringsAsFactors=F, 
                    colClasses = c()))
}

codes <- c('agglevel','industry','ownership','size')
ann2015full <- ann2015

n <- c(2:6,8)
na <- names(ann2015full)[n]
#convert class
for (i in na) {
  set(ann2015full, j=i, value = as.integer(ann2015full[[i]]))
  
}

# or
ann2015full[,(na):= lapply(.SD, as.integer), .SDcols=na]

for (i in 1:length(codes)) {
  tmp <- eval(parse(text=codes[i]))
  code <- names(tmp)[1]
  if (class(ann2015full[[code]]) != class(tmp[,1])) {
    c <- class(tmp[,1])
    eval(parse(text = paste0("set(ann2015full, j=code, value = as.", 
                             c, "(ann2015full[[code]]))")))
  }
  eval(parse(text=paste('ann2015full <- left_join(ann2015full,',codes[i],')', sep='')))
  
}


simpleCap <- function(x) {
  if (!is.na(x)) {
    s <- strsplit(x, " ")[[1]]
    
    result <- paste(toupper(substring(s,1,1)), substring(s,2),
                    sep='', collapse = " ")
    return(result)
    
  } else {return(NA)}
  
}


data("county.fips")
county.fips$fips <- str_pad(county.fips$fips, width=5,
                            pad="0")

county.fips$polyname <- as.character(county.fips$polyname)

county.fips$county <- sapply(
  gsub('[a-z\ ]+,([a-z\ ]+)','\\1',county.fips$polyname),
  simpleCap)
county.fips <- unique(county.fips)
data(state.fips)
state.fips$fips <- str_pad(state.fips$fips, width=2, pad="0",
                           side='left')

state.fips$state <- as.character(state.fips$polyname)

state.fips$state <-
  gsub("([a-z\ ]+):[a-z\ \\']+",'\\1',state.fips$state)

state.fips$state <- sapply(state.fips$state, simpleCap)
mystatefips <-unique(state.fips[,c('fips','abb','state')])

lower48 <-
  setdiff(unique(state.fips$state),c('Hawaii','Alaska'))

myarea <- merge(area, county.fips,
                by.x='area_fips',by.y='fips', all.x=T)

myarea$state_fips <- substr(myarea$area_fips, 1,2)

myarea <- merge(myarea,
                mystatefips,by.x='state_fips',by.y='fips', all.x=T)

ann2015full <- left_join(ann2015full, myarea)

ann2015full <- filter(ann2015full, state %in% lower48)

#save the data
save(ann2015full, file='ann2014full.rda',compress=T)


#
d.state <- filter(ann2015full, agglvl_code==50)
d.state <- select(d.state, state, avg_annual_pay,
                  annual_avg_emplvl)

d.state$wage <- cut(d.state$avg_annual_pay,
                    quantile(d.state$avg_annual_pay, c(seq(0,.8, by=.2), .9, .95,
                                                       .99, 1)),
                    include.lowest=TRUE)

d.state$empquantile <- cut(d.state$annual_avg_emplvl,
                           quantile(d.state$annual_avg_emplvl,
                                    c(seq(0,.8,by=.2),.9,.95,.99,1)),
                           include.lowest=TRUE)

x <- quantile(d.state$avg_annual_pay, c(seq(0,.8,by=.2),.9,
                                        .95, .99, 1))
xx <- paste(round(x/1000),'K',sep='')
Labs <- paste(xx[-length(xx)],xx[-1],sep='-')
levels(d.state$wage) <- Labs

x <- quantile(d.state$annual_avg_emplvl,
              c(seq(0,.8,by=.2),.9, .95, .99, 1))
xx <- ifelse(x>1000, paste(round(x/1000),'K',sep=''),
             round(x))
Labs <- paste(xx[-length(xx)],xx[-1],sep='-')
levels(d.state$empquantile) <- Labs


Discretize <- function(x, breaks=NULL){
  if(is.null(breaks)){
    breaks <- quantile(x, c(seq(0,.8,by=.2),.9, .95, .99, 1))
    if (sum(breaks==0)>1) {
      temp <- which(breaks==0, arr.ind=TRUE)
      breaks <- breaks[max(temp):length(breaks)]
    }
  }
  x.discrete <- cut(x, breaks, include.lowest=TRUE)
  breaks.eng <- ifelse(breaks > 1000,
                       paste0(round(breaks/1000),'K'),
                       round(breaks))
  Labs <- paste(breaks.eng[-length(breaks.eng)], breaks.eng[-1],
                sep='-')
  levels(x.discrete) <- Labs
  return(x.discrete)
}

d.cty <- filter(ann2015full, agglvl_code==70)%>%
  select(state,county,abb, avg_annual_pay, annual_avg_emplvl)%>%
  mutate(wage=Discretize(avg_annual_pay),
         empquantile=Discretize(annual_avg_emplvl))

#visualizing
state_df <- map_data('state')
county_df <- map_data('county')

transform_mapdata <- function(x) {
  names(x)[5:6] <- c('state', 'county')
  for (u in c('state', 'county')) {
    x[,u] <- sapply(x[,u],simpleCap)
  }
  return(x)
}

state_df <- transform_mapdata(state_df)
county_df <- transform_mapdata(county_df)

chor <- left_join(state_df, d.state, by='state')

ggplot(chor, aes(long,lat,group=group)) +
  geom_polygon(aes(fill=wage))+geom_path(color='black',size=0.2) + 
  scale_fill_brewer(palette='PuRd') +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x='',y='', fill='Avg Annual Pay')



chor <- left_join(county_df, d.cty)

ggplot(chor, aes(long,lat, group=group))+
  geom_polygon(aes(fill=wage))+
  geom_path( color='white',alpha=0.5,size=0.2)+
  geom_polygon(data=state_df, color='black',fill=NA)+
  scale_fill_brewer(palette='PuRd')+
  labs(x='',y='', fill='Avg Annual Pay')+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())



#Exploring where the jobs are, by industry
d.sectors <- filter(ann2015full, industry_code %in%
                      c(11,21,54,52),
                    own_code==5, # Private sector
                    agglvl_code == 74 # county-level
                    )%>%
  select(state,county,industry_code, own_code,agglvl_code,
         industry_title, own_title, avg_annual_pay,
         annual_avg_emplvl)%>%
  mutate(wage=Discretize(avg_annual_pay),
         emplevel=Discretize(annual_avg_emplvl))
d.sectors <- filter(d.sectors, !is.na(industry_code))

chor <- left_join(county_df, d.sectors)
chor <- filter(chor, !is.na(industry_title))
ggplot(chor, aes(long,lat,group=group))+
  geom_polygon(aes(fill=emplevel))+
  geom_polygon(data=state_df, color='black',fill=NA)+
  scale_fill_brewer(palette='PuBu')+
  facet_wrap(~industry_title, ncol=2, as.table=T)+
  labs(fill='Avg Employment Level',x='',y='')+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())


#Animating maps for a geospatial time series

#download the data files
for (i in years) {
  Url <- "http://www.bls.gov/cew/data/files/year/csv/year_annual_singlefile.zip"
  f <- "year_annual_singlefile.zip"
  fileUrl <- gsub("year", i, Url)
  f <- gsub("year",i, f)
  pathfile <- paste0("./data/", f)
  download.file(fileUrl, destfile = pathfile)
  
}


get_data <- function(year) {
  #download data
  Url <- "http://www.bls.gov/cew/data/files/year/csv/year_annual_singlefile.zip"
  zipfile <- gsub("year", year,"year_annual_singlefile.zip")
  fileUrl <- gsub("year", year, Url)
  filepath <- file.path('data',zipfile)
  download.file(fileUrl, destfile = filepath)
  
  
  #load data
  unzip(file.path('data',zipfile),exdir = 'data')
  csvfile <-gsub('zip','csv',zipfile)
  csvfile <- gsub('_', '.', csvfile)
  
  dat <- fread(file.path('data',csvfile))
  data <- left_join(dat, myarea)
  
  dat <- filter(dat, agglvl_code==70)%>%
    select(state, county, avg_annual_pay)
  
  return(dat)
}

years <- 2003:2015
n <- length(years)
dat_list <- vector('list',n)
for (i in 1:n) {
  dat_list[[i]] <- get_data(years[i])
  
  names(dat_list)[i] <- substr(files[i],1,4)
}

get_data <- function(csvfile) {
  
  dat <- fread(file.path('data', csvfile)) # read data
  dat <- left_join(dat, myarea)
  dat <- filter(dat, agglvl_code==70) %>% 
  select(state, county, avg_annual_pay) 
  return(dat)
}

files <- dir('data',pattern = "annual.singlefile.csv")
n <- length(files)
dat_list <- vector('list',n)
for (i in 1:n) {
  dat_list[[i]]<- get_data(files[i])  # ingest data
  names(dat_list)[i] <- substr(files[i],1,4) #label list with
  
}




annpay <- ldply(dat_list)
breaks <- quantile(annpay$avg_annual_pay,
                   c(seq(0,.8,.2),.9,.95,.99,1))

mychoro <- function(d, fill_label='Avg Annual Pay'){
  # d has a variable "outcome" that
  # is plotted as the fill measure
  chor <- left_join(county_df, d)
  plt <- ggplot(chor, aes(long,lat, group=group))+
    geom_polygon(aes(fill=outcome))+
    geom_path(color='white',alpha=0.5,size=0.2)+
    geom_polygon(data=state_df, color='black',fill=NA)+
    scale_fill_brewer(palette='PuRd')+
    labs(x='',y='', fill=fill_label)+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
  return(plt)
}

plt_list <- vector('list',n)
for(i in 1:n){
  dat_list[[i]] <- mutate(dat_list[[i]],
                          outcome=Discretize(avg_annual_pay,breaks=breaks))
  plt_list[[i]] <-
    mychoro(dat_list[[i]])+ggtitle(names(dat_list)[i])
}

choroplethr_animate(plt_list)

#plot for myself
multiplot(plt_list[[1]],plt_list[[n]],cols = 2)


opload <- benchmark(
  CSV=read.csv('2015.annual.singlefile.csv',
               stringsAsFactors=F),
  CSVZIP=read.csv(unz('2015_annual_singlefile.zip',
                      '2015.annual.singlefile.csv'), stringsAsFactors=F),
  LOAD = load('ann2014full.rda'),
  FREAD = fread('2015.annual.singlefile.csv'),
  order='relative', # Report in order from shortest to longest
  replications=5
)

opload



CSVZIP=read.csv(unzip('data/2015_annual_singlefile.zip',
                    '2015.annual.singlefile.csv'), stringsAsFactors=F)


unzip('data/2015_annual_singlefile.zip','2015.annual.singlefile.csv',exdir = 'data')



