# Rmocks

Wanted to provide contained coverage for your R code. RMocks enables mocking on methods of R5 classes

## Installation

Clone the git repo and run R CMD INSTALL rmocks 

or

use the install_git function from devtools(https://github.com/hadley/devtools) : install_git("https://github.com/jpsimonroy/rmocks")

## Usage
library("testthat")

context("Examples")

before_each=function(){
	setRefClass("IntegrationClass", 
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


Use this only if you need to replace a method defn. 
Restrain from this usage if possible.

context("spy and expects")

test_that("Create a spy and mock a method", {
	before_each();
	model=spy("IntegrationClass", list(method1=function(){
		return("Something else");
	}));
	expect_equal(model$method1(), "Something else");
});

test_that("Use mocks to assert method invocations",{
	before_each();
	model=spy("IntegrationClass");

	model_fixed_params=model$expects("method1", list(1,2), "Return");
	model_fixed_params=model_fixed_params$expects("method2", list("arg1", "arg2"), "Arg1&Arg2");
	# invalid method invocation 
	expect_error(model_fixed_params$method1());
	# call matches expectations
	expect_equal(model_fixed_params$method1(1,2), "Return");
	# method2 expected to be called but never
	expect_false(model_fixed_params$realize_expectations());
	# call the method which was expected.
	expect_equal(model_fixed_params$method2("arg1", "arg2"), "Arg1&Arg2")
	# all expectations are matched now.
	expect_true(model_fixed_params$realize_expectations());


	# setup method invocation match with * params
	model_wild_match=model$expects("method2", returns="Default")
	expect_equal(model_wild_match$method2(1), "Default");
	expect_equal(model_wild_match$method2(1,2), "Default");
	expect_equal(model_wild_match$method2(), "Default");
	expect_true(model_fixed_params$realize_expectations());
});

context("stub and stubs")

Since no assertions are being made, this should be ok.

test_that("Create a stub and stub a method", {
	before_each();
	model=stub("IntegrationClass", list(method1=function(){
		return("Something else");
	}));
	expect_equal(model$method1(), "Something else");
});

test_that("Use stubs to match arguments and return values",{
	before_each();
	model_strict=stub("IntegrationClass", strict=T);
	model=stub("IntegrationClass");

	model_stubbed_strict=model_strict$stubs("method1", list(1,2), "Return");
	model_stubbed_strict=model_stubbed_strict$stubs("method2", list("arg1", "arg2"), "Arg1&Arg2");
	# invalid method invocation 
	expect_error(model_stubbed_strict$method1());
	# call matches expectations
	expect_equal(model_stubbed_strict$method1(1,2), "Return");
	expect_equal(model_stubbed_strict$method2("arg1", "arg2"), "Arg1&Arg2")

	model_stubbed=model$stubs("method1", list(1,2), "Return");
	model_stubbed=model_stubbed$stubs("method2", list("arg1", "arg2"), "Arg1&Arg2");
	# invalid method invocation returns NA
	expect_true(is.na(model_stubbed$method1()));

	# call matches expectations
	expect_equal(model_stubbed$method1(1,2), "Return");
	expect_equal(model_stubbed$method2("arg1", "arg2"), "Arg1&Arg2")


	# setup method invocation match with * params
	model_wild_match=model$stubs("method2", returns="Default")
	expect_equal(model_wild_match$method2(1), "Default");
	expect_equal(model_wild_match$method2(1,2), "Default");
	expect_equal(model_wild_match$method2(), "Default");
});

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
