
#
# NAP issue reporting
#

output$issue.table <- renderDataTable({
  req( attached.edf() , values$issuesData )
  values$issuesData
},
  rownames = FALSE,
  options = list( pageLength=20, rownames=F , columnDefs = list(list( className="dt-center", targets = "_all" ) ) )
)
