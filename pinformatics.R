####August 31th came up with the idea of a Infomatics page
###Sept 6th, file was created and attempts at hacks were carried out but unsuccessful
####Sept 12th, successfully wrote a section of script to access the Pin Borad URLS of a User
####Sept 13th, the data was not sucessfully when extracting Pin Board Names
####Sept 21st, major data issues were fixed, all that is left is extracting Likes, 
####Repins and the sum of followers from Re-Pinner. Issue with speed. Project suspended until Oct. 17th 
library(XML)
pinformatics = function(userURL){
User.info = htmlParse(userURL, useInternal = TRUE)
###Above step allows the access to the user profile, I will add custom error
Pin.Board.URL = as.character(getNodeSet(User.info, "//a[@class = 'boardLinkWrapper']/@href"))
Pin.Board.URL = paste("http://pinterest.com", Pin.Board.URL, sep = "")
num.of.Pin.Board.URL = length(Pin.Board.URL)
Pin.Board.Name = sapply(strsplit(Pin.Board.URL,"/"), tail, n=1)
Pin.Count = as.numeric(gsub("[^0-9]*", "", sapply(getNodeSet(User.info, "//div[@class='PinCount Module']"), xmlValue))) 
Pin.URL = c()
ID.Pin.Board = c()
Repin.URL = c()
Pinner = c()
Pin.Origin = c()
for(i in 1:num.of.Pin.Board.URL){
    Pin.Board.Info = htmlParse(Pin.Board.URL[i], useInternal = TRUE)
    sudo.URL = as.character(getNodeSet(Pin.Board.Info, "//a[@class = 'pinImageWrapper ']/@href"))
    sudo.URL = paste("http://pinterest.com", sudo.URL, sep = "")
    sudo.URL = sudo.URL[sudo.URL!="http://pinterest.com"]
    sudo.Repin.URL = paste(sudo.URL, "repins/", sep = "")
    Pin.URL = c(Pin.URL, sudo.URL)
    Repin.URL = c(Repin.URL, sudo.Repin.URL)
    
    ####issues
     for(i in 1:length(sudo.URL)){
       Pinned.URL = htmlParse(sudo.URL[i], useInternal = TRUE)
       Repined.URL = htmlParse(sudo.Repin.URL[i], useInternal = TRUE)
       print(sudo.URL[i])
       Pined.Board = ifelse(is(try(as.character(sapply(getNodeSet(Pinned.URL, "//h4[@class = 'boardRepSubtitle']"), xmlValue))
                                   , silent=T), "try-error"), "Unknown Pinboard",
                            as.character(sapply(getNodeSet(Pinned.URL, "//h4[@class = 'boardRepSubtitle']"), xmlValue)))
       print(Pined.Board)
       ID.Pin.Board = c(ID.Pin.Board, Pined.Board)
       Pin.Author = ifelse(is(try(as.character(sapply(getNodeSet(Pinned.URL, "//h4[@class = 'fullname']"), xmlValue))
                                  , silent=T), "try-error"), "Unknown Author",
                           as.character(sapply(getNodeSet(Pinned.URL, "//h4[@class = 'fullname']"), xmlValue)))
       Pinner = c(Pinner,Pin.Author)
       Pin.Source = ifelse(is(try(as.character(sapply(getNodeSet(Pinned.URL, "//h4[@class = 'richPinSourceWrapper']"), xmlValue))
                                  , silent=T), "try-error"), "Unknown Origin",
                           as.character(sapply(getNodeSet(Pinned.URL, "//h4[@class = 'richPinSourceWrapper']"), xmlValue)))
      Pin.Origin = c(Pin.Origin,Pin.Source)
      Repin.Analysis.current.pin = 
        ifelse(is(try(as.character(getNodeSet(Repined.URL, "//a[@class = 'boardLinkWrapper']/@href"))
                                                 , silent=T), "try-error"), "No Repin URL",
               paste("http://pinterest.com", 
                     as.character(getNodeSet(Repined.URL, "//a[@class = 'boardLinkWrapper']/@href")), "followers/", sep=""))
# #       Repin.Analysis.current.pin = Repin.Analysis.current.pin[Repin.Analysis.current.pin != "http://pinterest.com/followers/"]
# #       RepinFollows.pin = c()
# #       for(i in 1:length(Repin.Analysis.current.pin)){
# #         RepinDoc = htmlParse(Repin.Analysis.current.pin, useInternal = TRUE)
# #       
# #       ###errors
# #         RepinFollows.pin = ifelse(is(try(getNodeSet(RepinDoc, ""), 
# #                                      silent=T), "try-error"), 0,
# #           sapply(getNodeSet(RepinDoc, "//a[@class ='active']"), xmlValue))
 ####apply t-text
# #              print(RepinFollows.pin)         
# #       }
# #       sum(RepinFollows.pin)
    }
}
#    ###Likes
#    ###Repins
#    ###The Number of user reached by Repins
 Pin.Summary = data.frame(Pin.URL,ID.Pin.Board
                          , Pinner, Pin.Origin)
return(Pin.Summary)
}