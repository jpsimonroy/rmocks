generate_class_name=function(class_name){
	str=format(Sys.time(), "%Y%H%M%OS6");
	paste(class_name,gsub('\\.', '', str), sep="");
};

spy=function(class_name, methods=list(), fields=list()){
	setRefClass(generate_class_name(class_name),
		contains=c(eval(class_name), "Mocker"),
		methods=methods,
		fields=fields,
		where=.GlobalEnv
	)$new();
};

stub=function(class_name, methods=list(), strict=F){
	ref=setRefClass(generate_class_name(class_name),
		contains=c(eval(class_name), "Stubber"),
		methods=methods,
		where=.GlobalEnv
	)$new();
	ref$strict=strict;
	ref;
};
