# Import lead and campaign data sets
# Data Cleaning for title

install.packages("dplyr")
library("dplyr")

## Transform to lower case and eliminate special characters
lead$Title <- lapply(lead$Title, FUN=tolower)
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("[^[:alnum:]///' ]", "", x))
lead$Title <- lapply(lead$Title, FUN=function(x) gsub(" ", "", x))

## Unify directors
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("assistantvp", "director", x))
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("associatevp", "director", x))
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("avp", "director", x))
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("associatevicepre", "director", x))
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("assistantvicepre", "director", x))
## Unify CEO
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("founder", "CEO", x))
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("owner", "CEO", x))
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("chiefexecutive", "CEO", x))
## Unify chief
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("vp", "chief", x))
lead$Title <- lapply(lead$Title, FUN=function(x) gsub("vicepre", "chief", x))

# Create features for title
Title <- lead

## board
Title$board <- 0
Title$board[grepl('Board',ignore.case=TRUE, Title$Title)]<-1
## chief
Title$chief <- 0
Title$chief[grepl('CAO', Title$Title)]<-1
Title$chief[grepl('CCO', Title$Title)]<-1
Title$chief[grepl('CFO', Title$Title)]<-1
Title$chief[grepl('CEO', Title$Title)]<-1
Title$chief[grepl('CIO', Title$Title)]<-1
Title$chief[grepl('CKO', Title$Title)]<-1
Title$chief[grepl('CLO', Title$Title)]<-1
Title$chief[grepl('CMO', Title$Title)]<-1
Title$chief[grepl('CNO', Title$Title)]<-1
Title$chief[grepl('COO', Title$Title)]<-1
Title$chief[grepl('CPO', Title$Title)]<-1
Title$chief[grepl('CQO', Title$Title)]<-1
Title$chief[grepl('CRO', Title$Title)]<-1
Title$chief[grepl('CSO', Title$Title)]<-1
Title$chief[grepl('CTO', Title$Title)]<-1
Title$chief[grepl('chief', ignore.case=TRUE,Title$Title)]<-1
Title$chief[grepl('president', ignore.case=TRUE,Title$Title)]<-1
## director
Title$director <- 0
Title$director[grepl('dir', ignore.case=TRUE,Title$Title)]<-1
Title$director[grepl('manag', ignore.case=TRUE,Title$Title)]<-1
## head
Title$head <- 0
Title$head[grepl('head', ignore.case=TRUE,Title$Title)]<-1
Title$head[grepl('lead', ignore.case=TRUE,Title$Title)]<-1
## consultant
Title$consultant <- 0
Title$consultant[grepl('consul', ignore.case=TRUE,Title$Title)]<-1
## business
Title$business <- 0
Title$business[grepl('marketing', ignore.case=TRUE,Title$Title)]<-1
Title$business[grepl('sales', ignore.case=TRUE,Title$Title)]<-1
## student
Title$student <- 0
Title$student[grepl('student', ignore.case=TRUE,Title$Title)]<-1
## technical
Title$technical <- 0
Title$technical[grepl('info', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('sys', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('IT', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('tech', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('data', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('software', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('developer', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('arch', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('analy', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('engineer', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('app', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('solution', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('virtual', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('visual', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('digi', ignore.case=TRUE,Title$Title)]<-1
Title$technical[grepl('cloud', ignore.case=TRUE,Title$Title)]<-1
## finalize
lead <- Title

# Data cleaning for country

## Transform to lower case and eliminate special characters
lead$Country <- lapply(lead$Country, FUN=tolower)
lead$Country <- lapply(lead$Country, FUN=function(x) gsub("[^[:alnum:]///' ]", "", x))
lead$Country <- lapply(lead$Country, FUN=function(x) gsub(" ", "", x))

# Create features for country
leadcountry <- lead
leadcountry$Country[grepl('Unitedstates',leadcountry$Country,ignore.case = TRUE)]<-"USA"
leadcountry$Country[grepl('US',leadcountry$Country,ignore.case = TRUE)]<-"USA"
leadcountry$Country[grepl('kingdom',leadcountry$Country,ignore.case = TRUE)]<-"UK"
leadcountry$Country[grepl('brita',leadcountry$Country,ignore.case = TRUE)]<-"UK"
leadcountry$Country[grepl('England',leadcountry$Country,ignore.case = TRUE)]<-"UK"
leadcountry$Country[grepl('Germa',leadcountry$Country,ignore.case = TRUE)]<-"Germany"
leadcountry$Country[grepl('ca',leadcountry$Country,ignore.case = TRUE)]<-"Canada"
leadcountry$Country[grepl('^$',leadcountry$Country)]<-"USA"
leadcountry$Country <- lapply(leadcountry$Country,FUN=function(x) gsub(" ", "", x))
lead <- leadcountry

# Data cleaning for company name

## To lower case, remove special characters and space
lead$Company...Account<- lapply(lead$Company...Account, FUN=tolower)
lead$Company...Account <- lapply(lead$Company...Account, FUN=function(x) gsub("[^[:alnum:]///' ]", "", x))
lead$Company...Account <- lapply(lead$Company...Account, FUN=function(x) gsub(" ", "", x))
lead$Company...Account <- lapply(lead$Company...Account, FUN=function(x) gsub("'", "", x))

# Merge country & company name
lead$Cleaned.company <- paste(lead$Company...Account,lead$Country,sep=",")

# Count unique
x <- unique(lead$Company...Account)

# Create features for company size
lead$companysize <- 0
lead$companysize <- cut(lead$No..of.Employees,
                        breaks <- c(0,1,101,1001,10001,50001,100001,200001,Inf),
                        labels <- c("0-1","1-100","101-1,000","1,001-10,000","10,001-50,000","50,001-100,000","100,001-200,000","200,000+"),
                        right=FALSE)
# Backup
lead1 <- lead 
# "lead1" is the original lead data set, "lead" below is the processed data set

# Remove duplicates
lead$oppor <- NA
lead$oppor <- ifelse(as.Date(lead$Opportunity..Created.Date, "%m/%d/%Y")>as.Date("1/1/1901", "%m/%d/%Y"),1,0)
lead <- lead[order(lead$Cleaned.company,-lead$oppor),]
lead <- lead[!duplicated(lead$Cleaned.company),]

# Count of unique companies in Campaign and Lead
count.lead <- as.data.frame(apply(lead,1,function(x)sum(lead1$Cleaned.company==x[41])))
lead <- cbind(lead,count.lead)
colnames(lead)[44] <- "Count in Lead"

## Deal with campaign data
### Company name
colnames(campaign)[11] <- "name"
campaign$name <- lapply(campaign$name, FUN=tolower)
campaign$name <- lapply(campaign$name, FUN=function(x) gsub("[^[:alnum:]///' ]", "", x))
campaign$name <- lapply(campaign$name, FUN=function(x) gsub(" ", "", x))
campaign$name <- lapply(campaign$name, FUN=function(x) gsub("'", "", x))
### Country
campaign$Country <- lapply(campaign$Country, FUN=tolower)
campaign$Country <- lapply(campaign$Country, FUN=function(x) gsub("[^[:alnum:]///' ]", "", x))
campaign$Country <- lapply(campaign$Country, FUN=function(x) gsub(" ", "", x))

campaign$Country[grepl('United states',campaign$Country,ignore.case = TRUE)]<-"USA"
campaign$Country[grepl('US',campaign$Country,ignore.case = TRUE)]<-"USA"
campaign$Country[grepl('kingdom',campaign$Country,ignore.case = TRUE)]<-"UK"
campaign$Country[grepl('brita',campaign$Country,ignore.case = TRUE)]<-"UK"
campaign$Country[grepl('England',campaign$Country,ignore.case = TRUE)]<-"UK"
campaign$Country[grepl('Germa',campaign$Country,ignore.case = TRUE)]<-"Germany"
campaign$Country[grepl('ca',campaign$Country,ignore.case = TRUE)]<-"Canada"
campaign$Country[grepl('^$',campaign$Country)]<-"USA"
campaign$Country <- lapply(campaign$Country,FUN=function(x) gsub(" ", "", x))
### Combine
campaign$Cleaned.company <- paste(campaign$name,campaign$Country,sep=",")

# select index and company name in lead
ids <- attr(lead,"row.names")
newLead <- cbind(data.frame(ids),lead$Cleaned.company)
idss <- attr(newLead,"row.names")
newl <- cbind(data.frame(idss),newLead$`lead$Cleaned.company`)
colnames(newl) <- c("id","Cleaned.company")

# Count in campaign
newCampaign <- rbind(campaign,campaign)
newCampaign <- rbind(newCampaign,campaign)
new <- left_join(newCampaign,newl,by = "Cleaned.company")
count.cam <- as.data.frame(apply(newl,1,function(x)sum(new$id==x[18])))
lead <- cbind(lead,count.lead)
new$ids <- paste(new$id)
new$ids[new$ids == "NA"] <- 0
count <- as.data.frame(table(new$ids))
newl$ids <- paste(newl$id)
count <- count[-1,]
colnames(count) <- c("ids","Count_in_Campaign")
count$Count_in_campaign <- count[,-1]/3
count <- count[,-2]
newll <- left_join(newl,count,by="ids")
newll$Count_in_campaign[is.na(newll$Count_in_campaign)] <-0
lead <- cbind(lead,newll$Count_in_campaign)
names(lead)[45] <- "Count in Campaign"

# Export data
Lead <- data.frame(lapply(lead, as.character), stringsAsFactors=FALSE)
write.csv(Lead,"LeadDataFinal")
getwd()