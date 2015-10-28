###2013
####August 31th came up with the idea of a Infomatics page after miserable internship experience
###Sept 6th, file was created and attempts at accessing Pinterest were carried out but unsuccessful
####Sept 12th, successfully wrote a section of script to access the Pin Borad URLS of a User
####Sept 13th, the data was not sucessfully when extracting Pin Board Names
####Sept 21st, major data issues were fixed, all that is left is extracting Likes, 
####Repins and the sum of followers from Re-Pinner_ Issue with speed_ Project suspended_ 
###2015
####Febuary 2nd: Project has been restarted, htmlParse doesn't work. 
####Febuary 3rd: do to https  security, workaround with the httr package, issues with the Pin analyzer (line 28-38) 
####Feburary 4th, Pin Analyzer fixed

library(XML)
library(RCurl)
library(httr)
pinformatics = function(userURL){
User_info =  htmlTreeParse(GET(userURL),useInternalNodes = T)
###Above step allows the access to the user profile, I will add custom error
Pin_Board_URL = paste("http://pinterest.com", as.character(getNodeSet(User_info, "//a[@class = 'boardLinkWrapper']/@href")), sep = "")
Pin_Board_Name = sapply(strsplit(Pin_Board_URL,"/"), tail, n=1)
Pin_Count = as.numeric(gsub("[^0-9]*", "", sapply(getNodeSet(User_info, "//div[@class='PinCount Module']"), xmlValue))) 
Pin_URL = c()
ID_Pin_Board = c()
Repin_URL = c()
Pinner = c()
Pin_Origin = c()
##This For loop should scrape from every board, information on each Pin's Re Pinner !Issue!
for(i in 1:length(Pin_Board_URL)){
    Pin_Board_Info = htmlParse(GET(Pin_Board_URL[i]), useInternal = TRUE)
    sudo_URL = as.character(getNodeSet(Pin_Board_Info, "//a[@class = 'pinImageWrapper']/@href"))
    sudo_URL = paste("http://pinterest.com", sudo_URL, sep = "")
    sudo_URL = sudo_URL[sudo_URL!="http://pinterest.com"]
    if(length(sudo_URL)>0){
      sudo_Repin_URL = paste(sudo_URL, "repins/", sep = "")
      Pin_URL = c(Pin_URL, sudo_URL)
      Repin_URL = c(Repin_URL, sudo_Repin_URL)
    }
}
  
    ####issues
#      for(i in 1:length(sudo_URL)){
#        Pinned_URL = htmlParse(GET(sudo_URL[i]), useInternal = TRUE)
#        Repined_URL = htmlParse(GET(sudo_Repin_URL[i]), useInternal = TRUE)
#        print(sudo_URL[i])
#        Pined_Board = ifelse(is(try(as.character(sapply(getNodeSet(Pinned_URL, "//h4[@class = 'boardRepSubtitle']"), xmlValue))
#                                    , silent=T), "try-error"), "Unknown Pinboard",
#                             as.character(sapply(getNodeSet(Pinned_URL, "//h4[@class = 'boardRepSubtitle']"), xmlValue)))
#        print(Pined_Board)
#        ID_Pin_Board = c(ID_Pin_Board, Pined_Board)
#        Pin_Author = ifelse(is(try(as.character(sapply(getNodeSet(Pinned_URL, "//h4[@class = 'fullname']"), xmlValue))
#                                   , silent=T), "try-error"), "Unknown Author",
#                            as.character(sapply(getNodeSet(Pinned_URL, "//h4[@class = 'fullname']"), xmlValue)))
#        print(Pin_Author)
#        Pinner = c(Pinner,Pin_Author)
#        Pin_Source = ifelse(is(try(as.character(sapply(getNodeSet(Pinned_URL, "//h4[@class = 'richPinSourceWrapper']"), xmlValue))
#                                   , silent=T), "try-error"), "Unknown Origin",
#                            as.character(sapply(getNodeSet(Pinned_URL, "//h4[@class = 'richPinSourceWrapper']"), xmlValue)))
#        print(Pin_Source)
#        Pin_Origin = c(Pin_Origin,Pin_Source)
# #       Repin_Analysis_current_pin = 
# #         ifelse(is(try(as.character(getNodeSet(Repined_URL, "//a[@class = 'boardLinkWrapper']/@href"))
# #                                                  , silent=T), "try-error"), "No Repin URL",
# #                paste("http://pinterest.com", 
# #                      as.character(getNodeSet(Repined_URL, "//a[@class = 'boardLinkWrapper']/@href")), "followers/", sep=""))
# # #       Repin_Analysis_current_pin = Repin_Analysis_current_pin[Repin_Analysis_current_pin != "http://pinterest.com/followers/"]
# # #       RepinFollows_pin = c()
# # #       for(i in 1:length(Repin_Analysis_current_pin)){
# # #         RepinDoc = htmlParse(Repin_Analysis_current_pin, useInternal = TRUE)
# # #       
# # #       ###errors
# # #         RepinFollows_pin = ifelse(is(try(getNodeSet(RepinDoc, ""), 
# # #                                      silent=T), "try-error"), 0,
# # #           sapply(getNodeSet(RepinDoc, "//a[@class ='active']"), xmlValue))
#  ####apply t-text
# # #              print(RepinFollows_pin)         
# # #       }
# # #       sum(RepinFollows_pin)
#     }
}
#    ###Likes
#    ###Repins
#    ###The Number of user reached by Repins
#  Pin_Summary = data.frame(Pin_URL,ID_Pin_Board
#                           , Pinner, Pin_Origin)
return(Pin_Summary)
}

