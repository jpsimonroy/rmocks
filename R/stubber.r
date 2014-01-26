Stubber=setRefClass("Stubber", 
	fields=list(
		method_meta="list",
		strict="logical"
	),
	contains="MockBase",
	methods=list(
		post_extension=function(stubber){
			strict<<-stubber$strict;
		},
		stubs=function(method_name, args=NULL, returns=NULL){
			extend_and_inject(method_name, args, returns);
		},
		generate_function=function(method_name){
			new_methods=list();
			new_methods[[method_name]]=function(...){
				currently_executing_function_name=attributes(sys.function(0))[["name"]];
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
				if(identical(args, method_expectations[[i]])){
					index=i;
					break;
				}
			}
			if(index==0){
				if(strict){
					stop("ERROR::Unexpected method invocation:", method_name, " with params ", paste(args, collapse=" "));
				}
				return(NA);
			}else{
				return(method_meta[[method_name]][["returns"]][[index]]);
			}
		},
		is_wild_expects=function(method_name){
			method_expectations=method_meta[[method_name]][["assertions"]];
			if(length(method_expectations) >= 1 && identical(method_expectations[[1]], list(NULL))){
				return(T);
			}
			return(F);
		}
	)		
);

