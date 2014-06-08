library("testthat")
source("../../R/r-mocks.r")
source("../../R/mock_base.r")
source("../../R/mocker.r")


context("The Mocker DSL")

before_each=function(){
	setRefClass("TestMocker", 
		fields=list(name="character"),
		methods=list(
			method1=function(){
				return("Method1");
			},
			method2=function(argument1, argument2){
				return(paste(argument1, argument2, sep=""))
			},
			method3=function(argument1, callback){
				callback(argument1);
			},
			method4=function(argument1){
				return(argument1);
			},
			method5=function(){
				method3(50, .self$method4);
			}
		),
	);
}

context("$expects")

test_that("creates a dynamic subclass and returns an instance of the newly created class",{
	before_each();
	model=spy("TestMocker");
	model=model$expects("method1");
	expect_that(model, is_a("TestMocker"));
});

test_that("takes assertions and persists them as instance vars", {
	before_each();
	model=spy("TestMocker");
	model=model$expects("method1", list(1,2));
	model=model$expects("method1", list(1,2,3));
	model=model$expects("method1", list());
	expect_that(model, is_a("TestMocker"));

	expect_true(identical(model[['method_meta']][['method1']][['assertions']], list(list(1,2), list(1,2,3), list())));
});

test_that("sets up empty invocations until methods are called", {
	before_each();
	model=spy("TestMocker");
	model=model$expects("method1", list(1,2));
	expect_true(is.null(model[['method_meta']][['method1']][['invocations']]));
});

test_that("returns a new instance which retains contract and also tracks method invocations when setup",{
	before_each();
	model=spy("TestMocker");
	model=model$expects("method1", list(1,2));
	model=model$expects("method1", list(1,2,3));
	model=model$expects("method1", list());
	
	model$method1(1,2);
	model$method1(1,2,3);
	expect_true(identical(model[['method_meta']][['method1']][['invocations']], list(list(1,2), list(1,2,3))));
	
	model$method1();
	
	expect_true(identical(model[['method_meta']][['method1']][['invocations']], list(list(1,2), list(1,2,3), list())));
});

context("interactions")

test_that("throws errors when methods are invoked with unexpected expectations", {
	before_each();
	model=spy("TestMocker");
	model=model$expects("method1", list(1,2));
	expect_error(model$method1());
});

test_that("matches functions when passed as arguments",{
	before_each();
	model=spy("TestMocker");
	model=model$expects("method3", list(50, model$method4));
	model$method5();

	expect_true(model$realize_expectations());
})

test_that("without argument should match all args",{
	before_each();
	model=spy("TestMocker");
	model=model$expects("method1",returns=list(1,2,3));
	expect_identical(model$method1(), list(1,2,3));
	expect_identical(model$method1(1,2), list(1,2,3));
});

test_that("without arguments should override existing expecations and return types",{
	before_each();
	model=spy("TestMocker");
	model=model$expects("method1", list("Argument"), returns=list(1,2,3));
	
	expect_identical(model$method1("Argument"),list(1,2,3));
	
	model=model$expects("method1",returns="HelloWorld");
	expect_match(model$method1("Argument"),"HelloWorld");
	expect_match(model$method1(2,3,4),"HelloWorld");
	
	model=model$expects("method1", list("Argument"), returns=list(1,2,3));
	expect_identical(model$method1("Argument"),list(1,2,3)) ;
});

test_that("should take a return value and return it when the mocked function is called",{
	before_each();
	model=spy("TestMocker");
	model=model$expects("method1",returns=list(1,2,3));
	expect_identical(model$method1(), list(1,2,3));

	model=model$expects("method1", args=list('a','b'), returns=list("r-mocks rocks"));
	expect_identical(model$method1('a', 'b'), list("r-mocks rocks"));
});

context("$generate_function")

test_that("should return a list of function definitions",{
	before_each();
	model=spy("TestMocker");
	new_methods=model$generate_function("test_method");

	expect_false(is.null(new_methods[["test_method"]]));
	expect_true(length(new_methods) == 1);
	expect_true(class(new_methods[["test_method"]]) == "function");	
});

context("$realize_expectations")

test_that("should return true if all expectations are met",{
	before_each();
	model=spy("TestMocker");
	model=model$expects("method1");
	model=model$expects("method2", list(1,2), "Hello");
	model$method1(1,2,3)
	model$method2(1,2)

	expect_true(model$realize_expectations());
});

test_that("should return false if all expectations are not met",{
	before_each();
	model=spy("TestMocker");
	model=model$expects("method1");
	model=model$expects("method2", list(1,2), "Hello");
	model$method1(1,2,3)
	
	expect_false(model$realize_expectations());

	model=spy("TestMocker");
	model=model$expects("method1");
	model=model$expects("method2", list(1,2), "Hello");
	model$method2(1,2);

	model$realize_expectations();
});