library("testthat")
source("../../R/r-mocks.r")
source("../../R/mock_base.r")
source("../../R/stubber.r")


context("The stubber DSL")

before_each=function(){
	setRefClass("TestStubber", 
		fields=list(name="character"),
		methods=list(
			method1=function(){
				return("Method1");
			},
			method2=function(argument1, argument2){
				return(paste(argument1, argument2, sep=""))
			}
		),
	);
}

context("$stub")

test_that("creates a dynamic subclass and returns an instance of the newly created class",{
	before_each();
	model=stub("TestStubber");
	model=model$stubs("method1");
	expect_that(model, is_a("TestStubber"));
});

test_that("strict mode is turned off by default",{
	before_each();
	model=stub("TestStubber");
	model=model$stubs("method1");
	expect_that(model, is_a("TestStubber"));
	expect_false(model$strict);
});


test_that("takes assertions and persists them as instance vars", {
	before_each();
	model=stub("TestStubber");
	model=model$stubs("method1", list(1,2));
	model=model$stubs("method1", list(1,2,3));
	model=model$stubs("method1", list());
	expect_that(model, is_a("TestStubber"));
	expect_true(identical(model[['method_meta']][['method1']][['assertions']], list(list(1,2), list(1,2,3), list())));
});

context("interactions")

test_that("throws errors when methods are invoked with unexpected expectations in strict mode", {
	before_each();
	model=stub("TestStubber", strict=T);
	model=model$stubs("method1", list(1,2));
	expect_error(model$method1());
});

test_that("should not throw errors when methods are invoked with unexpected expectations when not in strict mode", {
	before_each();
	model=stub("TestStubber");
	model=model$stubs("method1", list(1,2));
	model$method1();
});

test_that("without argument should match all args",{
	before_each();
	model=stub("TestStubber");
	model=model$stubs("method1",returns=list(1,2,3));
	expect_identical(model$method1(), list(1,2,3));
	expect_identical(model$method1(1,2), list(1,2,3));
});

test_that("without arguments should override existing expecations and return types",{
	before_each();
	model=stub("TestStubber");
	model=model$stubs("method1", list("Argument"), returns=list(1,2,3));
	
	expect_identical(model$method1("Argument"),list(1,2,3));
	
	model=model$stubs("method1",returns="HelloWorld");
	expect_match(model$method1("Argument"),"HelloWorld");
	expect_match(model$method1(2,3,4),"HelloWorld");
	
	model=model$stubs("method1", list("Argument"), returns=list(1,2,3));
	expect_identical(model$method1("Argument"),list(1,2,3)) ;
});

test_that("should take a return value and return it when the mocked function is called",{
	before_each();
	model=stub("TestStubber");
	model=model$stubs("method1",returns=list(1,2,3));
	expect_identical(model$method1(), list(1,2,3));

	model=model$stubs("method1", args=list('a','b'), returns=list("r-mocks rocks"));
	expect_identical(model$method1('a', 'b'), list("r-mocks rocks"));
});

	
context("$generate_function")

test_that("should return a list of function definitions",{
	before_each();
	model=stub("TestStubber");
	new_methods=model$generate_function("test_method");

	expect_false(is.null(new_methods[["test_method"]]));
	expect_true(length(new_methods) == 1);
	expect_true(class(new_methods[["test_method"]]) == "function");	
});