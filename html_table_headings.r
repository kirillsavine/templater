html_table_headings=function(
i_data=chun,
i_col_names=c("2017-01-04", "2017-01-05"),
i_headings="ag",
i_child_rows="gr",
i_table_params="style='border: 1px solid black;'",
i_th_params="style='border: 1px solid black;'",
i_tr_params="",
i_td_params="style='border: 1px solid black;'",
i_headings_params="style='font-weight: bold;'",
i_upper_left_cell="",
i_collapse=TRUE,
i_browse=FALSE
){

	library(data.table)
	
	table_tag=paste0("<table ",i_table_params,">")
	th_tag=paste0("<th ",i_th_params,">")
	tr_tag=paste0("<tr ",i_tr_params,">")
	td_tag=paste0("<td ",i_td_params,">")
	
	
	i_data=as.data.table(i_data)

	i_data=i_data[, c(i_headings,i_child_rows,i_col_names), with = F]

	col_names=paste0("c_",gsub("[^[:alnum:] ]", "_", i_col_names))

	names(i_data)=c(i_headings,i_child_rows,col_names)

	col_names_h=paste0(td_tag,col_names,"</td>")

	et=function(x){eval(parse(text=x))}

	res=i_data[,.(
	 v=paste0(td_tag,paste(eval(parse(text=paste0("c(",toString(col_names),")"))),collapse=paste0("</td>",td_tag)),"</td>")
	), by = c(i_headings,i_child_rows)]

	ress=res[,.(
	 v=paste0(tr_tag,paste(td_tag,eval(parse(text=i_child_rows)),"</td>",v,collapse=paste0("</tr>",tr_tag)),"</tr><tr></tr>")
	), by = c(i_headings)]

	ress$vv=paste0(tr_tag,"<td ",i_headings_params," colspan='",length(col_names)+1,"'>",ress[,eval(parse(text=i_headings))],"</td></tr>",ress$v)

	res_table=c(
	#"<style>table, th, td {border: 1px solid black;}</style>",
	table_tag,
	paste0(tr_tag,th_tag,i_upper_left_cell,"</th>",th_tag,paste(i_col_names,collapse=paste0("</th>",th_tag)),"</tr>"),
	ress$vv,
	"</table>"
	)
	
	if(i_collapse==TRUE){result=paste(res_table,collapse="")}else{result=res_table}
	if(i_browse==TRUE){writeLines(result,"C:\\1.html")  ;browseURL("C:/1.html")}
	return(result)
}
