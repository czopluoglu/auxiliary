##########################################################################
# Ludlow, L. H., & O’leary, M. (1999). Scoring omitted and not-reached items: 
#   Practical data analysis implications. Educational and Psychological Measurement, 
#   59(4), 615-630.

# Missing data is considered in two categories:
#   a) skipped/omitted: a student reads the item but decides not to respond
#   b) not reached: a student doesn't have the opportunity to response due 
#      to lack of timing.

# Definition of not-reach items: a string of consecutive items without responses 
# continuing through to the end of the assessment.
# Any other missing item that is not part of this long string can be 
# considered as skipped.

# Coding not-reached item as incorrect may lead biased item parameters
# such as items look more difficult than they are. So, by leaving NR items
# as missing in the original item calibration procedure
# should provide more accurate item parameter estimation.

# Once the item parameter calibration is done, and raw score-scale score
# conversion tables are formed, we can still count non-reached items
# as incorrect for the purpose of assigning a scale score to each individual.

# This is Strategy 4 discussed in the paper above.

##########################################################################

na.NR <- function(data,pos){
  
  ##################################################
  # Inputs 
  #
  #   data --> either Tier1 or Tier2 data matrix in the scaling-items-coded\2020
  #             generated from Daniel's code
  #  
  #   pos  --> a numeric vector for the item column positions in the data
  #
  # Output
  # 
  #   data   --> data with missing values handled
  #   
  #   report --> a 2 column matrix reporting the percentate
  #             of Skipped and Not-Reached items
  #################################################
  
  #####################################################
  # a mini function that recodes the missing values
  # for a given vector.
  
  # It checks the longest NA string occuring at the end
  # of the vector and replaces them with 99 (Not Reached)
  # then replaces the remaining missing values not 
  # included in this string as 9 (Skipped/Omitted)
  # This function will return the same vector
  # if there is no missing data
  
     na.NR.mini <- function(x){
       
        # x is a numeric vector

        miss = rev(which(is.na(x)==TRUE))
        ref  = length(x):(length(x)-length(miss)+1)
        
        nr = miss[which(miss == ref)]
        
        if(length(nr)!=0) {
          x[nr]=99
          } # 99 is not reached
        
        if(length(which(is.na(x)))!=0) {
          x[which(is.na(x))] = 9   # 9 Skipped/omitted
        }
        
        x
  
      }
  #################################################
     
  # temporary item only data
  
  temp = as.matrix(data[,pos])
  
  # Recode -1 as missing
  
  temp[which(temp==-1)] = NA
 
  # Loop over rows
  # Identify the Not Reached and code them as 99
  # code Skipped as 9
  # This is for reporting purposes. 
  # At the end Skipped will be coded as 0
  # and Not Reached will be coded as NA
  
  for(i in 1:nrow(temp)){
    temp[i,] = na.NR.mini(temp[i,])
  }

    # Create a report table
  
      rep = as.data.frame(matrix(nrow=ncol(temp),ncol=2))
      rownames(rep) = colnames(temp)
      colnames(rep) = c("Skipped","NotReached")
      for(i in 1:ncol(temp)){
        rep[i,1]= length(which(temp[,i]==9))/nrow(temp)
        rep[i,2]= length(which(temp[,i]==99))/nrow(temp)
      }
      
      rep <- round(rep*100,2)
  
    
  # Recode the skipped as incorrect
      
      if(length(which(temp==9))!=0){
        temp[which(temp==9)] = 0  
      }
      
  # Recode the not reached as missing
      if(length(which(temp==99))!=0){
        temp[which(temp==99)] = NA
      }

      
  # replace the item data in the original data
      
      data[,pos] = temp
      
      
  return(list(data=data,report=rep))
         
}


###################################################################

# Demo
#   
# y = matrix((runif(1000,0,1)>.5)*1,nrow=100,ncol=10)
# 
# for(i in 1:10){
#   y[sample(1:100,i*5),i]=NA
# }
# 
# na.NR(y,pos=1:10)
  
  
  
  
  
  
  
  
  
  
  
  