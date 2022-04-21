#' Function to propcase character variable
propercase <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

#    Copyright (c) 2022 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
#
#    This file is part of the metalite.ae program.
#
#    metalite.ae is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Format AE listing analysis
#'
#' @param outdata a `outdata` object created by `prepare_ae_specific`
#' @param display_subline whether display subline or not.
#'
#' @export

format_ae_listing <- function(outdata,
                              display_subline=FALSE) {
  res <- outdata$ae_listing
  
  obs_group <- collect_adam_mapping(outdata$meta, outdata$observation)$group
  obs_id <- collect_adam_mapping(outdata$meta, outdata$observation)$id
  par_var <- collect_adam_mapping(outdata$meta, outdata$parameter)$var
  
  if(display_subline){
    # Subline
    # create a subject line with participant's demographic information.         
    # this is for page_by argument in rtf_body function 
    res$subline <- paste0("Gender = ",res$SEX,
                          ", Race = ",res$RACE, 
                          ", Age = ",res$AGE, " Years")
    
  }
  
  
  res$Gender <- propercase(res$SEX)
  res <- res[,!(names(res) == "SEX")]
  
  res$Race <- propercase(res$RACE)
  res <- res[,!(names(res) == "RACE")]
  
  res$Age <- propercase(res$AGE)
  res <- res[,!(names(res) == "AGE")]
  
  res$Treatment_Group <- propercase(res[[obs_group]])
  res <- res[,!(names(res) == obs_group)]
  
  
  # Onset Epoch
  if("EPOCH" %in% toupper(names(res))){
    res$EPOCH1 <- propercase(res$EPOCH) # propcase the EPOCH    
  }
  
  # Site ID
  if("SITEID" %in% toupper(names(res))){
    res$Site_ID <- propercase(res$SITEID)
    res <- res[,!(names(res) == "SITEID")]   
  }
  
  if("SITENUM" %in% toupper(names(res))){
    res$Site_ID <- propercase(res$SITENUM)
    res <- res[,!(names(res) == "SITENUM")]   
  }
  
  # Relative day of onset (ASTDY)
  if("ASTDY" %in% toupper(names(res))){
    res$Relative_Day_of_Onset <- res$ASTDY
    res <- res[,!(names(res) == "ASTDY")]   
  }
  
  # Adverse Event
  res$Adverse_Event <- propercase(res[[par_var]])
  res <- res[,!(names(res) == par_var)]
  
  # Duration
  if("ADURN" %in% toupper(names(res)) & "ADURU" %in% toupper(names(res))){
    res$Duration <- paste(ifelse(is.na(res$ADURN), "", as.character(res$ADURN)) ,propercase(res$ADURU),sep=" ") # AE duration with unit
    res <- res[,!(names(res) %in% c("ADURN", "ADURU"))]
  }
  
  # Serious
  if("AESER" %in% toupper(names(res))){
    res$Intensity <- propercase(res$AESER)
    res <- res[,!(names(res) == "AESER")]
  }
  
  # Intensity
  if("AESEV" %in% toupper(names(res))){
    res$Serious <- propercase(res$AESEV)
    res <- res[,!(names(res) == "AESEV")]
  }
  
  # AE related
  if("AEREL" %in% toupper(names(res))){  
    res$Related <- ifelse(res$AEREL == 'RELATED', "Y", ifelse(
      res$AEREL == 'NOT RELATED', "N", propercase(res$AEREL)
    ))
    
    res <- res[,!(names(res) == "AEREL")]
  }
  
  # Action taken
  if("AEACN" %in% toupper(names(res))){
    res$Action_Taken <- ifelse(
      res$AEACN == 'DOSE NOT CHANGED', 'None', ifelse(
        res$AEACN =='DOSE REDUCED', 'Reduced', ifelse(
          res$AEACN =='DRUG INTERRUPTED', 'Interrupted',ifelse(
            res$AEACN =='DOSE INCREASED', 'Increased',ifelse(
              res$AEACN =='NOT APPLICABLE', 'N/A', ifelse(
                res$AEACN =='UNKNOWN', 'Unknown', propercase(res$AEACN)))))))
    
    res <- res[,!(names(res) == "AEACN")]
  }
  
  
  # Outcome
  if("AEOUT" %in% toupper(names(res))){
    res$Outcome <- ifelse(
      res$AEOUT == 'RECOVERED/RESOLVED' , 'Resolved', ifelse(
        res$AEOUT == 'RECOVERING/RESOLVING', 'Resolving', ifelse(
          res$AEOUT =='RECOVERED/RESOLVED WITH SEQUELAE', 'Sequelae', ifelse(
            res$AEOUT =='NOT RECOVERED/NOT RESOLVED', 'Not Resolved',ifelse(
              res$AEOUT %in% c('FATAL', 'UNKNOWN') , propercase(res$AEOUT), propercase(res$AEOUT))))))
    
    res <- res[,!(names(res) == "AEOUT")]
  }
  
  
  outdata$ae_listing <- res
  
  outdata
}