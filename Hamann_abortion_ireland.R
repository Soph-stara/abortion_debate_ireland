#### 1. INSTALL PACKAGES ####
install.packages("load.Rdata")
library(tidyverse)
library(ggplot2)
library(quanteda)
library(dplyr)

#### 2. LOAD DATASET ####
load("irelandabortion.RData")

#TRANSFORM TO EASIER NAME
ireland_abortion<-ireland_speeches_abortion


#### 3. CREATE CORPUS ####

ireland_abortion_corp <- corpus(ireland_abortion, docid_field = "speechID",text_field = "speech")
str(ireland_abortion_corp)
        #### 3.1 SUBSET CORPUS ####
# "Ireland_abortion_corp" ist der Gesamtkorpus; "ireland_abortion_corpus" ist der Corpus nur mit den relevanten Daten
ireland_abortion_corpus <- corpus_subset(ireland_abortion_corp,date%in%c("1983-02-02",
                                                                         "1983-02-09",
                                                                         "1983-02-15",
                                                                         "1983-02-17",
                                                                         "1983-02-23",
                                                                         "1983-03-02",
                                                                         "1983-03-08",
                                                                         "1983-03-24",
                                                                         "1983-04-27",
                                                                         "1992-10-20",
                                                                         "1992-10-21",
                                                                         "1992-10-22",
                                                                         "1992-10-27",
                                                                         "2001-04-05",
                                                                         "2001-10-25",
                                                                         "2001-11-08",
                                                                         "2001-11-13",
                                                                         "2001-11-14",
                                                                         "2001-11-20",
                                                                         "2001-12-04",
                                                                         "2001-12-05",
                                                                         "2018-05-29"))
table(ireland_abortion_corpus$documents$date)
str(ireland_abortion_corpus)

#### 4. CREATE DFM ####
#ich gruppiere nach nichts! ich skaliere ungruppiert
dfm_ireland_abortion_corpus <- dfm(ireland_abortion_corpus,remove_punct=TRUE,
                             remove_numbers=TRUE,
                             remove=stopwords("english"),
                             stem=TRUE)
                             #groups="member_name")
                             #dictionary=data_dictionary_Rauh)
    #### 4.1 TRIMMING THE DFM####
dfm_ireland_abortion_corpus <- dfm_trim(dfm_ireland_abortion_corpus,min_termfreq = 5, min_docfreq = 25)

#convert to data frame
#dfm_ireland_abortion_corpus <- dfm_ireland_abortion_corpus %>% convert(to="data.frame")

#### 5. BERECHNEN WORDSCORE####
    #### 5.1 DEFINITION REFERENCE DOCUMENTS ####

        #### EXPERIMENTIEREN FÜR REFERENZTEXTE####
            ####subsetting corpus 1983 für die definition des Pro Life Referenztextes ####       

ireland_abortion_corpus_1983<-corpus_subset(ireland_abortion_corpus, date%in%c("1983-02-02",
                                                                               "1983-02-09",
                                                                               "1983-02-15",
                                                                               "1983-02-17",
                                                                               "1983-02-23",
                                                                               "1983-03-02",
                                                                               "1983-03-08",
                                                                               "1983-03-24",
                                                                               "1983-04-27"))
summary(ireland_abortion_corpus_1983)
show(ireland_abortion_corpus_1983$documents$texts)
2057505

            #### subsetting des corpus 2001/2018 für die definition des pro choice referenztextes####

ireland_abortion_corpus_2001<-corpus_subset(ireland_abortion_corpus, date%in%c("2001-04-05",
                                                                               "2001-10-25",
                                                                               "2001-11-08",
                                                                               "2001-11-13",
                                                                               "2001-11-14",
                                                                               "2001-11-20",
                                                                               "2001-12-04",
                                                                               "2001-12-05"))
summary(ireland_abortion_corpus_2018)
summary(ireland_abortion_corpus_2001$documents$texts)

#####Hier definieren wir die zwei extremen Reden (Pro Life und Pro Choice), müssen mit ihrer Nummer angegeben werden####
pro_life.number <- which(row.names(dfm_ireland_abortion_corpus)=="2057501")
pro_choice.number <- which(row.names(dfm_ireland_abortion_corpus)=="3238240")

    #### 5.2 CREATE VECTORS ####
#We then create a vector with NAs of length 2317 (the number of speeches) and replace the NA in the position of the two most opposite extreme speeches
wordscores.vec <- rep(NA,2282) 
wordscores.vec[pro_life.number] <- -1 
wordscores.vec[pro_choice.number] <- 1 
wordscores.vec

show(dfm_ireland_abortion_corpus)


    #### 5.3 RUNNING WORDSCORE FOR REFERENCE DOCS####
#Then, we run Wordscores and check the output.
ws_dfm_ireland_abortion_corpus <- textmodel_wordscores(x=dfm_ireland_abortion_corpus,y=wordscores.vec) 
summary(ws_dfm_ireland_abortion_corpus)

    #### SUBSETTING ORIGINAL DATASET FOR CBIND INTO "ireland_abortion_subset (damit Zeilenanzahl wieder gleich ist)####

ireland_abortion_subset <- subset(ireland_abortion,date%in%c("1983-02-02",
                                                             "1983-02-09",
                                                             "1983-02-15",
                                                             "1983-02-17",
                                                             "1983-02-23",
                                                             "1983-03-02",
                                                             "1983-03-08",
                                                             "1983-03-24",
                                                             "1983-04-27",
                                                             "1992-10-20",
                                                             "1992-10-21",
                                                             "1992-10-22",
                                                             "1992-10-27",
                                                             "2001-04-05",
                                                             "2001-10-25",
                                                             "2001-11-08",
                                                             "2001-11-13",
                                                             "2001-11-14",
                                                             "2001-11-20",
                                                             "2001-12-04",
                                                             "2001-12-05",
                                                             "2018-05-29")) 

    #### 5.4 APPLY WORDSCORE TO VIRGIN TEXT ####
ws_dfm_ireland_abortion_corpus_pred <- data.frame(predict(ws_dfm_ireland_abortion_corpus,se.fit=T, interval="confidence",rescaling="mv"))

#hier verbinden wir den predict Wortschatz mit dem ursprünglichen Wortschatz - vorsicht: wenn man das öfters durchlaufen lässt, werden die Variablen in dem Datensatz verdoppelt!
ws_dfm_ireland_abortion_corpus_pred <- (cbind(ws_dfm_ireland_abortion_corpus_pred, ireland_abortion_subset))


table(ireland_abortion_subset$date)


#test, ob die Variablen doppelt sind
names(ws_dfm_ireland_abortion_corpus_pred)
str(ws_dfm_ireland_abortion_corpus_pred)
show(ws_dfm_ireland_abortion_corpus_pred$speechID)
show(ws_dfm_ireland_abortion_corpus_pred$fit.lwr)
show(ws_dfm_ireland_abortion_corpus_pred$speech)



    #### 5.5 PLOTTING RESULT####
ggplot(data=ws_dfm_ireland_abortion_corpus_pred,aes(x=as.Date(date),y=fit.fit))+
  geom_point(size=1)+theme_bw()+
  labs(x="Year",y="Wordscores")+
  ggtitle("Change in Ideology over the Years")+
  coord_cartesian()+
  theme_minimal()


ggplot(data=ws_dfm_ireland_abortion_corpus_pred,aes(x=as.Date(date),y=fit.fit))+
  geom_point(size=0.5)+
  theme_bw()+labs(x="",y="Wordscores Position")

#### 6. BERECHNUNG DER RESULTS ####
    #### 6.1 SUBSETTING WS_PRED 1983 ####

ws_dfm_ireland_abortion_corpus_pred_1983 <- subset(ws_dfm_ireland_abortion_corpus_pred,date%in%c("1983-02-02",
                                                                                                 "1983-02-09",
                                                                                                 "1983-02-15",
                                                                                                 "1983-02-17",
                                                                                                 "1983-02-23",
                                                                                                 "1983-03-02",
                                                                                                 "1983-03-08",
                                                                                                 "1983-03-24",
                                                                                                 "1983-04-27")) 
table(ws_dfm_ireland_abortion_corpus_pred_1983$date)
table(ws_dfm_ireland_abortion_corpus_pred_1983$member_name)



        #### 6.1.1 BERECHNUNG MITTELWERT####
mean(ws_dfm_ireland_abortion_corpus_pred_1983$fit.fit)

        #### 6.1.2 BERECHNUNG VARIANZ####

var(ws_dfm_ireland_abortion_corpus_pred_1983$fit.fit)

    #### 6.2 SUBSETTING WS_PRED 1992 ####

ws_dfm_ireland_abortion_corpus_pred_1992 <- subset(ws_dfm_ireland_abortion_corpus_pred,date%in%c("1992-10-20",
                                                                                                 "1992-10-21",
                                                                                                 "1992-10-22",
                                                                                                 "1992-10-27")) 
table(ws_dfm_ireland_abortion_corpus_pred_1992$date)


        #### 6.2.1 BERECHNUNG MITTELWERT####

mean(ws_dfm_ireland_abortion_corpus_pred_1992$fit.fit)

        #### 8.2.2 BERECHNUNG VARIANZ####

var(ws_dfm_ireland_abortion_corpus_pred_1992$fit.fit)

    #### 6.3 SUBSETTING WS_PRED 2001 ####
table(ws_dfm_ireland_abortion_corpus_pred$date)

ws_dfm_ireland_abortion_corpus_pred_2001 <- subset(ws_dfm_ireland_abortion_corpus_pred,date%in%c("2001-04-05",
                                                                                                 "2001-10-25",
                                                                                                 "2001-11-08",
                                                                                                 "2001-11-13",
                                                                                                 "2001-11-14",
                                                                                                 "2001-11-20",
                                                                                                 "2001-12-04",
                                                                                                 "2001-12-05")) 
table(ws_dfm_ireland_abortion_corpus_pred_2001$date)


        #### 6.3.1 BERECHNUNG MITTELWERT####

mean(ws_dfm_ireland_abortion_corpus_pred_2001$fit.fit)

        #### 6.3.2 BERECHNUNG VARIANZ####

var(ws_dfm_ireland_abortion_corpus_pred_2001$fit.fit)
    #### 6.4 SUBSETTING WS_PRED 2018 ####

ws_dfm_ireland_abortion_corpus_pred_2018 <- subset(ws_dfm_ireland_abortion_corpus_pred,date%in%c("2018-05-29")) 
table(ws_dfm_ireland_abortion_corpus_pred_2018$date)
summary(ws_dfm_ireland_abortion_corpus_pred_2018)


        #### 6.4.1 BERECHNUNG MITTELWERT####

mean(ws_dfm_ireland_abortion_corpus_pred_2018$fit.fit)

        #### 6.4.2 BERECHNUNG VARIANZ####

var(ws_dfm_ireland_abortion_corpus_pred_2018$fit.fit)

#### 7. VISUALISIEREN DER ERGEBNISSE ####



#### 8. DESKRIPTIVE BESCHREIBUNG DES DATENSATZES ####
  #Wie viele Männer und wie viele Frauen haben in den jeweiligen Jahren eine Rede gehalten? (% Berechnen)
  #In welcher Debatte haben mehr Frauen und in welcher mehr Männer geredet? (Ist das möglich? da mehrere Themen am gleichen Datum)
table(ireland_abortion_corpus$documents$date)
    #### 8.1 JAHR 1983 ####

ireland_abortion_corpus_1983 <- corpus_subset(ireland_abortion_corpus,date%in%c("1983-02-02",
                                                                                "1983-02-09",
                                                                                "1983-02-15",
                                                                                "1983-02-17",
                                                                                "1983-02-23",
                                                                                "1983-03-02",
                                                                                "1983-03-08",
                                                                                "1983-03-24",
                                                                                "1983-04-27")) 


table(ireland_abortion_corpus_1983$documents$date)
table(ireland_abortion_corpus_1983$documents$texts)
table(ireland_abortion_corpus_1983$documents$title)
table(ireland_abortion_corpus_1983$documents$member_name)



    #### 8.2 JAHR 1992 ####

ireland_abortion_corpus_1992 <- corpus_subset(ireland_abortion_corpus,date%in%c("1992-10-20",
                                                                                "1992-10-21",
                                                                                "1992-10-22",
                                                                                "1992-10-27"))
table(ireland_abortion_corpus_1992$documents$member_name)
    #### 8.3 JAHR 2001 ####

ireland_abortion_corpus_2001 <- corpus_subset(ireland_abortion_corpus,date%in%c("2001-04-05",
                                                                                "2001-10-25",
                                                                                "2001-11-08",
                                                                                "2001-11-13",
                                                                                "2001-11-14",
                                                                                "2001-11-20",
                                                                                "2001-12-04",
                                                                                "2001-12-05"))

table(ireland_abortion_corpus_2001$documents$date)
table(ireland_abortion_corpus_2001$documents$member_name)
table(ireland_abortion_corpus_2001$documents$title)


    #### 8.4 JAHR 2018 ####

ireland_abortion_corpus_2018 <- corpus_subset(ireland_abortion_corpus,date%in%c("2018-05-29"))

names(ireland_abortion)


table(ireland_abortion_corpus_2018$documents$date)
table(ireland_abortion_corpus_2018$documents$member_name)
table(ireland_abortion_corpus_2018$documents$party_name)

table(ireland_abortion_corpus_2018$documents$title)


table(ireland_abortion$date)
