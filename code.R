Needed <- c("tm")   
install.packages(Needed, dependencies=TRUE)  
library(tm)

////////////////////////////////////////////////////////////////////////////

# Set the working directory
setwd("R text mining/mining output")

////////////////////////////////////////////////////////////////////////////

cname <- file.path("\R text mining", "Files to be mined")
cname

////////////////////////////////////////////////////////////////////////////

docs <- Corpus(DirSource(cname))
summary(docs)

////////////////////////////////////////////////////////////////////////////

docs <- tm_map(docs, removePunctuation)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
docs <- tm_map(docs, removeSpecialChars)

docs <- tm_map(docs, tolower)

for(j in seq(docs))   
   {   
     docs[[j]] <- gsub("/", " ", docs[[j]])   
     docs[[j]] <- gsub("–", " ", docs[[j]])   
     docs[[j]] <- gsub("“", " ", docs[[j]])   
     docs[[j]] <- gsub("”", " ", docs[[j]])   
     docs[[j]] <- gsub("‘", " ", docs[[j]])
     docs[[j]] <- gsub("file", " ", docs[[j]])   
     docs[[j]] <- gsub("<", " ", docs[[j]])
     docs[[j]] <- gsub(">", " ", docs[[j]])   
     docs[[j]] <- gsub("&", " ", docs[[j]])   
     docs[[j]] <- gsub("punitives", "punitive", docs[[j]])   
     docs[[j]] <- gsub("tribunals", "tribunal", docs[[j]])   
     docs[[j]] <- gsub("californias", "california", docs[[j]])   
     docs[[j]] <- gsub("negligent", "negligence", docs[[j]])   
     docs[[j]] <- gsub("breached", "breach", docs[[j]])   
     docs[[j]] <- gsub("breaches", "breach", docs[[j]])   
     docs[[j]] <- gsub("breaches", "breach", docs[[j]])   
     docs[[j]] <- gsub("sexually", "sexual", docs[[j]])   
     docs[[j]] <- gsub("discriminated", "discrimination", docs[[j]])   
     docs[[j]] <- gsub("discriminatory", "discrimination", docs[[j]])   
     docs[[j]] <- gsub("discriminationharassment", "discrimination", docs[[j]])   
     docs[[j]] <- gsub("discriminationharassment", "discrimination", docs[[j]])   
     docs[[j]] <- gsub("harassed", "harassment", docs[[j]])   
     docs[[j]] <- gsub("harassing", "harassment", docs[[j]])   
     docs[[j]] <- gsub("collusive", "collusion", docs[[j]])   
     docs[[j]] <- gsub("racially", "racial", docs[[j]]) 
     docs[[j]] <- gsub("securities class", "securities_class_action", docs[[j]])
     docs[[j]] <- gsub("security class", "securities_class_action", docs[[j]])
     docs[[j]] <- gsub(" sca ", "securities_class_action", docs[[j]])
     docs[[j]] <- gsub("Odalenclass action", "class_action", docs[[j]])
     docs[[j]] <- gsub("class actions", "class_action", docs[[j]])
     docs[[j]] <- gsub("class action", "class_action", docs[[j]])
     docs[[j]] <- gsub("business interruption", "business_interruption", docs[[j]])
     docs[[j]] <- gsub("samesex", "homosexual", docs[[j]])
  }   

docs <- tm_map(docs, removeWords, c("atlanta", "axis", "insured", "gross", "policy", "incurred", "net", "claim", "coverage", "will", "claims", "million", "loss", "insurance", "reserve", "the", "excess", "primary", "matter", "inc", "limit", "claimant", "company", "alleges", "limits", "notice", "damages", "underlying", "retention", "alleging", "securexcess"))

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("english"))

library(SnowballC)   
docs <- tm_map(docs, stemDocument)  

docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, PlainTextDocument)

////////////////////////////////////////////////////////////////////////////

tdm <- TermDocumentMatrix(docs)
tdm

freq <- colSums(as.matrix(tdm))   
length(freq)
ord <- order(freq)

m2 <- as.matrix(tdm)   
dim(m2)   
write.csv(m2, file="NOTES_text_mining.csv")

////////////////////////////////////////////////////////////////////////////

Needed <- c("Wordcloud")   
install.packages(Needed, dependencies=TRUE)
library(wordcloud)

////////////////////////////////////////////////////////////////////////////

dtm <- DocumentTermMatrix(docs)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(table(freq), 20)
tail(table(freq), 20)
findFreqTerms(dtm, lowfreq=200)
wordcloud(names(freq), freq, min.freq=200, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

////////////////////////////////////////////////////////////////////////////

install.packages("ggplot2")
install.packages('Scales', dependencies = TRUE)
install.packages('colorspace', dependencies = TRUE)
library(ggplot2)

wf <- data.frame(word=names(freq), freq=freq)
head(wf)

p <- ggplot(subset(wf, freq>500), aes(x = reorder(word, -freq), y = freq)) +
+     geom_bar(stat = "identity") + 
+     theme(axis.text.x=element_text(angle=45, hjust=1))
p

////////////////////////////////////////////////////////////////////////////

text <- readLines("R text mining/Files/Descriptions from NOTES table_CMS - ex PRF.rpt")
docs <- Corpus(VectorSource(text))
tdm <- TermDocumentMatrix(docs)
findAssocs(tdm, c("excess"), corlimit=0.3)
