#Required Libraries
library(bitops)
library(RCurl)
library(XML)
library(stringr)
library(httr)
library(readr)

#Supress warnings if any
options(warn=-1)


#Set the working directory
Path = ("C:/Users/jayso/Documents/Crawler")

setwd(Path)

#Url of the main page
site = "https://www.gmb.org.br/copia-online-version"

#Few Strings which are used later to concatenation operations
secureLink = "https://"
unsecureLink = "http://"

#Creating required variables
webLinks = c()

#Using Rvest library functions to get the links
library(rvest)
url = "https://www.gmb.org.br/copia-online-version"
article.links = url %>% read_html() %>% xml_find_all("//*[@class=\"txtNew\"]/p/span/a") %>% html_attr("href")

#Final Output from Page 1
webLinks1 = unique(article.links[1:15])

#Links from the second page 
library(rvest)
url = "https://www.gmb.org.br/online-version"
article.links.1 = url %>% read_html() %>% xml_find_all("//*[@class=\"txtNew\"]/p/span/a") %>% html_attr("href")
article.links.2 = url %>% read_html() %>% xml_find_all("//*[@class=\"c4\"]/div/div/p/span/span/a") %>% html_attr("href")

article.links4 = c(article.links.1,article.links.2)

#Final Output from Page 2
webLinks2 = unique(article.links4)

#Storing correct links in a file
webLinks = c(webLinks1, webLinks2)
webLinks = webLinks[c(1:14, 16:48,50:86)]

write.csv(webLinks, file = "Links_to_all_yearwise_issues.csv", append = TRUE)

length(webLinks)

#Getting the exact paperlinks
paperlinks = c()

for (i in 1:length(webLinks)) {
  paperlinks = append(paperlinks,webLinks[i] %>% read_html() %>% xml_find_all("/html/body/div/table/tbody/tr/td/table/tbody/tr/td/div/a[2]") %>% html_attr("href"))
  print(i)
}
length(paperlinks)

#Removing the unwanted links
paperlinks = paperlinks[-1]
paperlinks = unique(paperlinks)
paperlinks = paperlinks[c(1:40,42:76,79:101,103:151,154:212,215:249,251:269,272:286,289:306,309:373,378:397,399:562,564:593,595:661,663:698,701:783,786:802,804:874,876:906,910:1004,1006:1092,1095:1110,1112:1116,1121:1167,1169:1204,1206:1257,1260:1305,1309:1389,1392:1486,1495:1517,1520:1579,1585:1610,1613:1661,1663:1712,1714:1730,1732:1756,1758:1778,1780:1797,1801:1819,1822:1875,1877:1897,1900:1941,1943:1948,1950:1984,1987:2038,2040:2049,2068:2103,2105:2113,2116:2142,2144:2166)]

length(paperlinks)
typeof(paperlinks)

#Storing the links in a csv file
write.csv(paperlinks, file = "real-links.csv", append = TRUE)

#Creating a blank DataFrame
iterateDataFrame = data.frame("DOI_data" =c(), "title" =c(), "author.list" =c(), "authorAffiliation" = c(), "correspondingAuthor" = c(), "abstract" = c(), "keywords.list" = c(), "fullText" = c())

#Main Crawl function with the obtained paperlinks
crawl_article = function(s.year){
    
    yearlink = paperlinks[grep(c(s.year),paperlinks)]
    
    #FUNCTION TO FETCH REQUIRED DATA
    fdata <- function(links){
      
      
      if(status_code(GET(links))==500)
        next
        doc = links %>% read_html()
        
        
        #Fetch DOI
        DOI_data = doc %>% html_nodes("body > div:nth-child(2) > div.content > h3") %>% html_text()
        #if(length(DOI_data)==0) DOI_data <- NA
        #if(grepl(date,DOI_data)==FALSE)
          #return(NA)
        #print(DOI_data)
        
        #Fetch Title
        title = doc %>% html_nodes("#article-front > div:nth-child(2) > p") %>% html_text()
        #print(title)
        
        #Fetch Author
        author.list = doc %>% html_nodes("#article-front > div:nth-child(2)") %>% html_text()
        #print(author.list)
        
      
        #Fetch Author Affiliations
        authorAffiliation.d = doc %>% html_nodes("#article-front > p") %>% html_text()
        authorAffiliation = paste(unlist(authorAffiliation.d),collapse = ",")
        #print(authorAffiliation.d)
        
        #Fetch Corresponding Author
        correspondingAuthor = doc %>% html_nodes("body > div:nth-child(2) > div.content > div.index\\,en > div.foot-notes > div > p") %>% html_text()
        #print(correspondingAuthor)
        
        #Fetch Abstract
        abstract = doc %>% html_nodes("#article-front > div.trans-abstract > p:nth-child(2)") %>% html_text()
        #print(abstract)
        
        #Fetch Keywords
        keywords.listd = doc %>% xml_find_all("//*[@id=\"article-front\"]/div/p[3]") %>% html_text()
        keywords.list = paste(unlist(keywords.listd),collapse = ",")
        #print(keywords.listd)
        
        #Fetch Fulltext
        fullText = paste("http://www.scielo.br",doc %>% html_nodes("#toolBox > div:nth-child(5) > ul > li:nth-child(3) > a") %>% html_attr("href"),sep = "")
        #print(fullText)
        
        #Removing all the error links by giving NA
        if(length(doc)==0) data.frame("DOI_data" = c(NA), "title" = c(NA), "author.list" = c(NA), "authorAffiliation" = c(NA), "correspondingAuthor" = c(NA), "abstract" = c(NA), "keywords.list" = c(NA), "fullText" = c(NA))
        if(length(title)==0) title <- NA
        if(length(author.list)==0) author.list <- NA
        if(length(authorAffiliation)==0) authorAffiliation <- NA
        if(length(correspondingAuthor)==0) correspondingAuthor <- NA
        if(length(abstract)==0) abstract <- NA
        if(length(keywords.list)==0) keywords.list <- NA
        if(length(fullText)==0) fullText <- NA
        
        #DATAFRAME WITH REAL VALUES
        iterateDataFrame = data.frame(DOI_data, title, author.list, authorAffiliation, correspondingAuthor, abstract, keywords.list, fullText)
        
        return(iterateDataFrame)
      }
    
    
    
        for (i in 1:length(yearlink)){
          x = fdata(yearlink[i])
          
          if (!is.na(x))
          
          iterateDataFrame = rbind(iterateDataFrame,x)
        print(i)
      }
      return(iterateDataFrame)
}



otpt = crawl_article("2018")

#When you run the program for different years, Don't forget to run the line 72 of this code.
#This will erase the previous entries in the Dataframe. 
#And hence, you will be able store new values in it. 

otpt = unique(otpt)
length(otpt)
nrow(otpt)

#Change the last 4 digitis of the filename to the year you are running the code for.
write.csv(otpt, file = "Crawl-Output-file2018.csv", append = TRUE)


#Idea to optimize it further. 
#Give this program a user input about years and then tell it to crawl data accoridng to the year input by the user. 


