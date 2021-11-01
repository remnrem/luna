

output$header.summary <- renderTable({

  req(attached.edf())
  
  k <- values$eval$HEADERS$BL
  k$EPOCH <- k$TOT_DUR_SEC / 30.0

  df <- data.frame( t( k ) )

  df$VAR <- c(
      "ID",
      "ID (EDF header)",
      "EDF Type",
      "Number of records",
      "Number of selected signals",
      "Total number of signals",
      "Record duration (secs)",
      "Start date",
      "Start time",
      "Duration (hh:mm:ss)",
      "Duration (secs)",
      "Duration (epoch)"
    )

  # return value
  df[,c(2,1)]  
  } ,
  width = '100%' , rownames = F , colnames = F , striped = T
 )
  


output$header.channels <- renderDataTable({

  req(attached.edf())

  k <- values$eval$HEADERS$CH

  k$ID <- NULL

  k <- k[, c("CH", "SR", "PDIM", "TRANS" , "PMIN", "PMAX","TYPE")]

  names(k) <- c("Channel" , "Sample rate", "Unit", "Transducer" , "Minimum", "Maximum", "Type" )

  # return value
  k
 } ,
  rownames = FALSE,
  options = list( pageLength=20, rownames=F , columnDefs = list(list( className="dt-center", targets = "_all" ) ) )
)


#
# Mapping tables
#

output$channel.mapping1 <- renderTable({
  req(attached.edf())
  values$chmap1
  df <- values$chmap1[ , c(2,3,6,5,4) ]
  names(df) <- c( "Harmonized" , "Defined" , "Original" , "Re-referenced" , "Notes" ) 
  df0 <- df[ df$Defined == 0 , ]
  df1 <- df[ df$Defined == 1 , ] 
  df <- rbind( df1[ order( toupper(df1$Harmonized) ) , ] , df0[ order( toupper(df0$Harmonized ) ) , ] )
  df
 } , width = '100%' , rownames = F , colnames = T , striped = T )


output$channel.mapping2 <- renderTable({
  req(attached.edf())
  df <- values$chmap2
  df <- df[ order( df$CH ) , ] 
  df <- df[ df$USED == 0 , ] 
  names(df)[2] <- "Original" 
  df$Original
 } , width = '100%' , rownames = F , colnames = F , striped = T )


output$channel.base.mapping1 <- renderTable({
  req(attached.edf())
  df <- values$bchmap1[ , c(2,3,6,5,7,8,4) ]
  names(df) <- c( "Harmonized" , "Defined" , "Original" , "Re-referenced" , "SR", "Units", "Notes" ) 
  df0 <- df[ df$Defined == 0 , ]
  df1 <- df[ df$Defined == 1 , ] 
  df <- rbind( df1[ order( toupper(df1$Harmonized) ) , ] , df0[ order( toupper(df0$Harmonized ) ) , ] )
  df
 } , width = '100%' , rownames = F , colnames = T , striped = T )


output$channel.base.mapping2 <- renderTable({
  req(attached.edf())
  df <- values$bchmap2
  df <- df[ order( df$CH ) , ] 
  df <- df[ df$USED == 0 , ] 
  names(df)[2] <- "Original" 
  df$Original
 } , width = '100%' , rownames = F , colnames = F , striped = T )


output$annot.mapping1 <- renderTable({
  req(attached.edf(),values$amap1)
  df <- values$amap1  
  names(df) <- c( "Class" , "Instance" , "Mapped" )
  df[ order( df$Mapped, toupper(df$Class) , toupper(df$Instance) ) , ] 
  
 } , width = '100%' , rownames = F , colnames = T , striped = T )

output$annot.mapping2 <- renderTable({
  req(attached.edf(),values$amap2)
  df <- values$amap2  
  names(df) <- c( "Original" , "Alias" )  
  df[ order( toupper( df[,1] ) ) , ] 
 } , width = '100%' , rownames = F , colnames = T , striped = T )

