
# i_path_to_template=paste0(report_path,"\\static\\t_01.html")
# i_01="1"
# i_02=c("1","2","3","4")

fis=function(pat,x){gregexpr(pat,x)[[1]][1]}

append_repl=function(l,inj,num){
	res=append(l, inj, after=num)
	res=res[-num]
	res
}
			


render_template=function(i_path_to_template="templates/1.html",...){
	
	my_args=list(...)
	for(x in 1:length(my_args)){eval(parse(text=paste0(names(my_args[x]),"=my_args[[x]]")))}	
	
	#print(i_01)
	
	
	l=readLines(i_path_to_template)
	l_clone=l

	echo_occur=grep("\\{\\{",l)
	if(length(echo_occur)>0){	
		for(i in 1:length(echo_occur)){		#		i=1
			st=echo_occur[i]
			en=grep("\\}\\}",l[st:length(l)])[1]+st-1
			item=l[st:en]
			item=item[!(item %in% c(""))]
			item=paste(item,collapse="")
			
			item_befo=substr(item,1,fis("\\{\\{",item)-1)
			item_core=substr(item,fis("\\{\\{",item),fis("\\}\\}",item)+1)
			item_afte=substr(item,fis("\\}\\}",item)+2,nchar(item)+1)
			item_core=gsub("^\\s+|\\s+$","",gsub("\\{\\{","",gsub("\\}\\}","",item_core)))

			
			item_val=paste0(item_befo,toString(get(item_core)),item_afte)
			
			if(length(st:en)>1){
				l_clone[st]=item_val
				l_clone[(st+1):en]=""
			}else{
				l_clone[st:en]=item_val
			}	
		}
	}

	eval_occur=grep("\\{\\%",l_clone)
	if(length(eval_occur)>0){	
		for(i in 1:length(eval_occur)){		#		i=2

			#st=eval_occur[i]
			st=grep("\\{\\%",l_clone)
			en=grep("\\%\\}",l_clone[st:length(l_clone)])[1]+st-1
			
			item=l_clone[st:en]
			item=item[!(item %in% c(""))]
			item=paste(item,collapse=";")
			
			item_befo=substr(item,1,fis("\\{\\%",item)-1)
			item_core=substr(item,fis("\\{\\%",item)+2,fis("\\%\\}",item)-1)
			item_afte=substr(item,fis("\\%\\}",item)+2,nchar(item)+1)			
			
			#item_core=gsub("^\\s+|\\s+$","",gsub("\\{\\%","",gsub("\\%\\}","",item)))
			#item_core=item_core[!(item_core %in% c("",item_befo,item_afte))]
			
			

			
			eval(parse(text=paste(c("funct=function(){",item_core,"};"),collapse=";")))
			res=c(item_befo,unlist(funct()),item_afte)
			
			if(length(st:en)>1){
				l_clone[(st+1):en]=""	
				l_clone=append_repl(l_clone,res,st)		
			}else{
				l_clone=append_repl(l_clone,res,st)	
			}	

		}
	}

	paste(l_clone,collapse="")
	return(l_clone)

}
