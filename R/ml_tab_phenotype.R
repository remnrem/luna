#Phenotype files are expected to be listed in the nap.dir/nap/ folder as 
# _pheno-*RData files where * represents 0 or more characters.
#
# Merge of phenotypes from multiple files is based on "ID" column matched with selected EDF(Sample) ID


output$pheno.table <- renderDataTable({
  req(attached.edf(),length(values$phenoData)!=0)
  v <-1
  df_final <- NULL
  for(i in 1:length(values$phenoData)){
    df<- values$phenoData[[i]]
    indiv_values <- df[df$ID == values$ID, ]
    if(is.data.frame(indiv_values) && !nrow(indiv_values) == 0){
      if(v == 1){
        df_final <-indiv_values
      } else{
        df_final <-merge(df_final,indiv_values,by="ID")
      }
      v <- v+1
    }
  }
  if(is.data.frame(df_final) && !nrow(df_final) == 0){
    df_final <- as.data.frame(t(df_final[,-1]))
    df_final$Variable <- rownames(df_final)
    df_final <-df_final[c(2,1)]
    colnames(df_final)[2] <-"Value"
  }
  df_final
  },
  rownames = FALSE,
  options = list( pageLength=20, rownames=F , columnDefs = list(list( className="dt-center", targets = "_all" ) ) )
)
