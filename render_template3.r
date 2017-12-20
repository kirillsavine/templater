

render_template3=function(i_path_to_template,...){
	my_args=list(...)
	x =readLines(i_path_to_template)
	
	my_args
	x_new=c()
	
	x[1:10]
		
	for(i in 1:length(x)){		#			i=33
		lin=x[i]
		
		for(j in 1:length(my_args)){		# j=1
			cur_arg_n=names(my_args)[j]
			cur_arg_v=my_args[[cur_arg_n]]

			locs=as.data.frame(str_locate_all(lin,"\\{\\{\\s*"+cur_arg_n+"\\s*\\}\\}")[[1]])
			
		
#			locs=data.frame("start"=c(1,2,3,4,5,6),"end"=c(10,20,30,40,50,60))
			if(nrow(locs)>0){
				if(nrow(locs)==1){
					surr_content_stops=c(0+"~"+locs$start,locs$end+1+"~"+nchar(lin)+1)
				}else{
					surr_content_stops=c(0+"~"+locs$start[1],locs$end[1:(length(locs$end)-1)]+1+"~"+locs$start[2:length(locs$start)],locs$end[length(locs$end)]+1+"~"+nchar(lin)+1)
				}
				surr_content=c()
				for(k in 1:length(surr_content_stops)){		#			k=2
					stops=as.numeric(strsplit(surr_content_stops[k],"~")[[1]])
					surr_content=c(surr_content,substr(lin,stops[1],stops[2]-1))
				}
				lin=paste(surr_content,collapse=cur_arg_v)
			}
		
		}
		x_new=c(x_new,lin)
		
	}
	
	
	x_new

}



