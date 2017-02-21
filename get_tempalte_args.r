
get_tempalte_args=function(x,directive=c("\\{\\{","\\}\\}")){


	mypat=function(pat,x){as.numeric(gregexpr(pat,x)[[1]])}
	
	mypos=function(pat,x,positions){
		res=lapply(x,function(y){mypat(pat,y)})
		names(res)=positions
		res
	}
	
	get_item=function(x,st,en){
		substr(x,st,en)
	}

	positions_st=grep(directive[1],x)
	dat_st=x[positions_st]

	positions_en=grep(directive[1],x)
	dat_en=x[positions_en]
	
	starts=mypos(directive[1],dat_st,positions_st)
	ends=mypos(directive[2],dat_en,positions_en)


	positions= data.frame(
		pos = rep(names(positions_within), sapply(positions_within, length)),
		st = unlist(starts),
		en = unlist(ends)
	)	
	
	positions=data.frame(lapply(positions,as.numeric),stringsAsFactors=FALSE)
	positions$item=substr(dat_st[positions$pos],positions$st+2,positions$en-1)
	
	positions
	#gsub(paste0(".*",directive[1],"\\s*|",directive[2],".*"), "", dat)
}

