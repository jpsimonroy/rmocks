library("testthat")
source("../../R/r-mocks.r")
source("../../R/mock_base.r")
source("../../R/stubber.r")
source("../../R/mocker.r")

context("The mock/stub mother goose");

before_each=function(){
	setRefClass("MotherGoose", 
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

context("$is_wild_expects")

test_that("should return true if the first expectation is NULL",{
	before_each();
	model_stub=stub("MotherGoose");
	model_stub=model_stub$stubs("method1");
	model_stub=model_stub$stubs("method2", list(), "Hello");

	expect_true(model_stub$is_wild_expects("method1"))

	model_mock=stub("MotherGoose");
	model_mock=model_mock$stubs("method1");
	model_mock=model_mock$stubs("method2", list(), "Hello");

	expect_true(model_mock$is_wild_expects("method1"))
	
});

test_that("should return false if expectation is set for a method",{
	before_each();
	model_mock=spy("MotherGoose");
	model_mock=model_mock$expects("method1");
	model_mock=model_mock$expects("method2", list(), "Hello");

	expect_false(model_mock$is_wild_expects("method2"))

	model_stub=spy("MotherGoose");
	model_stub=model_stub$expects("method1");
	model_stub=model_stub$expects("method2", list(), "Hello");

	expect_false(model_stub$is_wild_expects("method2"))
});
