

scan_template=function(x,directive=c("\\{\\{","\\}\\}")){
	op=directive[1]
	cl=directive[2]
		
	xx=ifelse(grepl(op,x) & grepl(cl,x),0,
		ifelse(grepl(op,x) & !grepl(cl,x),1,
			ifelse(!grepl(op,x) & grepl(cl,x),2,
				ifelse(!grepl(op,x) & !grepl(cl,x),3,4)
			)
		)
	)
	dat=data.frame(x=x,code=xx,stringsAsFactors=FALSE)
	dat[1:50,]

	dat_copy=data.frame(lapply(dat,as.character),stringsAsFactors=FALSE)
	flag=0			#			i=19
	for(i in 1:nrow(dat)){

		curcode=dat$code[i]
		print(paste0("curcode: ",curcode," flag: ",flag))

		if(flag==0){
			if(curcode %in% c(0,3,4)){
				dat_copy[i,"res"]=dat$x[i]
			}else if(curcode==1){
				flag=1
				#print(paste0("AAAAAAAAAA",flag))
				dat_copy[i,"res"]=dat$x[i]
			}else{
				stop(paste0("scanning error: closing directive ",cl," on line ", i))
			}
		}else{
			if(curcode %in% c(3,4)){
				dat_copy[i-flag,"res"]=paste(dat_copy[i-flag,"res"],"\n",dat$x[i],collapse="\n")
				dat_copy[i,"res"]=""
				flag=flag+1
			}else if(curcode==2){
				dat_copy[i-flag,"res"]=paste(dat_copy[i-flag,"res"],"\n",dat$x[i],collapse="\n")
				dat_copy[i,"res"]=""
				flag=0		
			}else if(curcode %in% c(0,1)){
				stop(paste0("scanning error: nested directive ",op," on line ", i))
			}
		}
		#print(paste0("BBBBBBBBB",flag))
	}


	dat_copy$res

}



mypat=function(pat,x){as.numeric(gregexpr(pat,x)[[1]])}

mypos=function(pat,x,positions){
	res=lapply(x,function(y){mypat(pat,y)})
	names(res)=positions
	res
}
	
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

get_tempalte_args=function(x,directive=c("\\{\\{","\\}\\}")){
	op=directive[1]
	cl=directive[2]
	
	x=scan_template(x,c(op,cl))
	positions_overall=grep(paste0(op,"|",cl),x)
	dat_overall=x[positions_overall]
	
	positions_st=grep(directive[1],x)
	dat_st=x[positions_st]

	positions_en=grep(directive[1],x)
	dat_en=x[positions_en]
	
	starts=mypos(directive[1],dat_st,positions_st)
	ends=mypos(directive[2],dat_en,positions_en)


	positions= data.frame(
		pos = rep(names(starts), sapply(starts, length)),
		st = unlist(starts),
		en = unlist(ends)
	)	
	
	positions=data.frame(lapply(positions,function(x){as.numeric(as.character(x))}),stringsAsFactors=FALSE)
	positions$item=trim(substr(x[positions$pos],positions$st+2,positions$en-1))
	
	list("profile"=positions,"scanned"=x)
	#gsub(paste0(".*",directive[1],"\\s*|",directive[2],".*"), "", dat)
}

eval_code=function(x, directive, ...){

	my_args=list(...)
	for(h in 1:length(my_args)){eval(parse(text=paste0(names(my_args[h]),"=my_args[[h]]")))}	
	

	if(directive[1]=="\\{\\%"){
		script_content=paste(c("funct=function(){",x,"};"))
		fil=tempfile();cat(fil)
		writeLines(script_content,fil)
		source(file=fil)
		res=funct()
		res=toString(unlist(funct()))
		unlink(fil,force = TRUE)
	}else if(directive[1]=="\\{\\{"){
		var_name=toString(trim(gsub(";","",x)))
		var_val=ifelse(exists(var_name),get(var_name),"")
		res=toString(var_val)
	}
	res
}


quotemeta = function(x) {
  library(stringr)
  str_replace_all(x, "(\\W)", "\\\\\\1")
}	
	
		
render_direct=function(x,directive=c("\\{\\{","\\}\\}"),...){

	op=directive[1]
	cl=directive[2]

	my_args=list(...)
	for(h in 1:length(my_args)){eval(parse(text=paste0(names(my_args[h]),"=my_args[[h]]")))}	
	
	dat=get_tempalte_args(x,directive=c(op,cl))
	dat_p=dat[[1]]
	dat_t=dat[[2]]
	
	#	j=1
	for(j in 1:nrow(dat_p)){		#		j=1
		my_item_info=dat_p[j,]
		my_line=dat_t[dat_p$pos[j]]
		my_eval=eval_code(x=my_item_info$item,directive=c(op,cl),...)
		#dat_t[dat_p$pos[j]]=gsub(paste0(op," *",quotemeta(my_item_info$item)," *",cl),my_eval,my_line,perl=TRUE)
		dat_t[dat_p$pos[j]]=my_eval
	}
	dat_t
}

render_template2=function(i_path_to_template,...){
	my_args=list(...)
	for(h in 1:length(my_args)){eval(parse(text=paste0(names(my_args[h]),"=my_args[[h]]")))}	
	#browser()
	x=readLines(i_path_to_template)
	p1=render_direct(x,directive=c("\\{\\%","\\%\\}"),...)
	p2=render_direct(p1,directive=c("\\{\\{","\\}\\}"),...)
	p2
}

