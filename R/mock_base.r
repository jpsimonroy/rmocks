MockBase=setRefClass("MockBase", 
	fields=list(method_meta="list"),
	methods=list(
		post_extension=function(mock_base){},
		add_to_list=function(existing_list, value, by_val=F){
			if(length(existing_list) == 0 ||is.null(existing_list) || is.na(existing_list)){
				existing_list=list();
			}
			existing_list[[length(existing_list) + 1]]<-if(by_val)  lapply(value, copy) else value
			existing_list
		},
		extend_and_inject=function(method_name, args=NULL, returns=NULL){
			extension_class_name=generate_class_name(class(.self)[[1]]);
			obj=extension(extension_class_name, .self$generate_function(method_name));
			obj$method_meta<-method_meta;
			new_assertions=obj[['method_meta']][[method_name]][["assertions"]];
			new_returns=obj[['method_meta']][[method_name]][["returns"]];
			if(is.null(args)){
				new_assertions=add_to_list(list(), list(NULL));
				new_returns=add_to_list(list(), if(is.null(returns)) list(NULL) else returns);
			}else{
				if(length(new_assertions) >= 1 && identical(new_assertions[[1]], list(NULL))){
					new_assertions=list();
					new_returns=list();
				}
				new_assertions=add_to_list(new_assertions, args);
				new_returns=add_to_list(new_returns, if(is.null(returns)) list(NULL) else returns);
			}
			obj[['method_meta']][[method_name]][["assertions"]]=new_assertions
			obj[['method_meta']][[method_name]][["returns"]]=new_returns;
			obj$post_extension(.self);
			obj

		},
		extension=function(new_class_name, methods, fields=list()){
			class_name=class(.self)[[1]]
			setRefClass(new_class_name, 
				contains=class_name,
				methods=methods,
				fields=fields,
				where=.GlobalEnv
			)$new();
		},
		is_wild_expects=function(method_name){
			method_expectations=method_meta[[method_name]][["assertions"]];
			if(length(method_expectations) >= 1 && identical(method_expectations[[1]], list(NULL))){
				return(T);
			}
			return(F)
		}
	)		
);

