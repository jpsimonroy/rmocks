library("testthat")
source("../../R/r-mocks.r")
source("../../R/mock_base.r")
source("../../R/mocker.r")
source("../../R/stubber.r")

context("Gateway to rmocks");

test_that(".generate_class_name should generate new class names",{
	expect_false(generate_class_name("TestClass") == generate_class_name("TestClass"))
});


test_that(".spy should generate a dynamic subclass which extends Mocker",{
	setRefClass("TestClass");
	model=spy("TestClass")
	expect_that(model, is_a("TestClass"));
	expect_that(model, is_a("Mocker"));
});

test_that(".spy should generate a dynamic subclass and override methods passed",{
	setRefClass("TestGateway", 
		fields=list(value="numeric"),
		methods=list(
			initialize=function(){
				value<<-5;
			},
			get_value=function(){
				return(value);
			},
			non_overridden_method=function(){
				return("Am not overriden");
			}
		)
	);

	model=spy("TestGateway",method=list(get_value=function(){return(10)}));
	model$value=100;
	expect_that(model, is_a("TestGateway"))
	expect_that(model$get_value(), equals(10));
	expect_that(model$non_overridden_method(), matches("Am not overriden"));
});