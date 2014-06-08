Mocker<-setRefClass("Mocker", 
	fields=list(method_meta="list"),
	contains="MockBase",
	methods=list(
		expects=function(method_name, args=NULL, returns=NULL){
			extend_and_inject(method_name, args, returns);
		},
		generate_function=function(method_name){
			new_methods=list();
			new_methods[[method_name]]=function(...){
				currently_executing_function_name=attributes(sys.function(0))[["name"]];
				method_meta[[currently_executing_function_name]][["invocations"]]<<-add_to_list(method_meta[[currently_executing_function_name]][["invocations"]], list(...), by_val=T);
				return(match_method_returns(currently_executing_function_name, list(...)));
			}
			new_methods;
		},
		match_method_returns=function(method_name, args){
			method_expectations=method_meta[[method_name]][["assertions"]];
			if(is_wild_expects(method_name)){
				return(method_meta[[method_name]][["returns"]][[1]]);
			}
			index=0;
			for(i in 1:length(method_expectations)){
				if(identical(args, method_expectations[[i]]) || isTRUE(all.equal(args, method_expectations[[i]]))){
					index=i;
					break;
				}
			}
			if(index==0){
				stop("ERROR::Unexpected method invocation:", method_name, " with params ", paste(args, collapse=" "));
				return(NA);
			}else{
				return(method_meta[[method_name]][["returns"]][[index]]);
			}
		},
		realize_expectations=function(){
			match=T;
			for(method_name in names(method_meta)){
				if(!match_expectations(method_meta[[method_name]][['assertions']], unlist(method_meta[[method_name]][['invocations']], recursive=F))){
					warning(paste("invocations on ", method_name," not matched"))
					match=F;
					break;
				}
			}
			return(match);
		},
		match_expectations=function(assertions, invocations){
			if(length(assertions) > 1 && identical(assertions[[1]], list(NULL)) && length(invocations) > 0){
				return(T);
			}
			invocations=unique(invocations);
			if(length(invocations) < length(assertions)){
				return(F);
			}
			return(T);
		}
	)		
);

