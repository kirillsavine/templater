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

		melt_list=function(x){as.list(setNames(unlist(x, use.names=F),rep(names(x), lengths(x))))}
		
		trim <- function (x) gsub("^\\s+|\\s+$", "", x)

		
        positions_st=grep(directive[1],x)
        dat_st=x[positions_st]

        positions_en=grep(directive[1],x)
        dat_en=x[positions_en]
        
        starts=melt_list(mypos(directive[1],dat_st,positions_st))
        ends=melt_list(mypos(directive[2],dat_en,positions_en))

        positions= data.frame(
              #  pos = rep(names(positions_within), sapply(positions_within, length)),
              #  pos = as.numeric(as.character(names(starts))),
                pos = names(starts),
                st = unlist(starts),
                en = unlist(ends)
        )       
        
		dat_st
		dat_en
		length(dat_st)
		
        positions2=data.frame(lapply(positions,as.numeric),stringsAsFactors=FALSE)
    

		positions2$item=trim(substr(dat_st[positions2$pos],positions2$st+2,positions2$en-1))
        
        positions2$pos=positions$pos
		
		positions2
        #gsub(paste0(".*",directive[1],"\\s*|",directive[2],".*"), "", dat)
}

