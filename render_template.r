

render_template=function(i_path_to_template="templates/1.html",...){
	
	my_args=list(...)
	for(x in 1:length(my_args)){eval(parse(text=paste0(names(my_args[x]),"=my_args[[x]]")))}	
	
	
	
	
	l=readLines(i_path_to_template)
	l_clone=l

	echo_occur=grep("\\{\\{",l)

	for(i in 1:length(echo_occur)){		#		i=4
		st=echo_occur[i]
		en=grep("\\}\\}",l[st:length(l)])[1]+st-1
		item=l[st:en]
		item=gsub("^\\s+|\\s+$","",gsub("\\{\\{","",gsub("\\}\\}","",item)))
		item=item[item!=""]
		
		if(length(st:en)>1){
			l_clone[st]=toString(get(item))
			l_clone[(st+1):en]=""
		}else{
			l_clone[st:en]=toString(get(item))
		}	
	}

	eval_occur=grep("\\{\\%",l)

	for(i in 1:length(eval_occur)){		#		i=3

		st=eval_occur[i]
		en=grep("\\%\\}",l[st:length(l)])[1]+st-1
		item=l[st:en]
		item=gsub("^\\s+|\\s+$","",gsub("\\{\\%","",gsub("\\%\\}","",item)))
		item=item[item!=""]
		
		
		eval(parse(text=paste(c("funct=function(){",item,"};"),collapse="")))
		res=paste(funct(),collapse="")
		
		
		if(length(st:en)>1){
			l_clone[st]=res
			l_clone[(st+1):en]=""	
		}else{
			l_clone[st:en]=res
		}	

	}

	paste(l_clone,collapse="")

}


