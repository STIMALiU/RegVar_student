
context("simple_lm()")

test_that("simple_lm", {  
  
  body_contain<-function(object,expected) {any(grepl(x = as.character(body(object)), pattern = expected))}
  package_loaded<-function(object){any(grepl(object, search()))}
  
  
  
  expect_true(exists("simple_lm"),
              info = "Fel: simple_lm() saknas.")
  
  expect_true(is.function(simple_lm),
              info = "Fel: simple_lm() är inte en funktion.")
  
  expect_function_self_contained(object = simple_lm,
                                 "Fel: Funktionen innehåller fria variabler = definerade utanför funktionen")
  
  expect_function_arguments(object = simple_lm,expected = c("x","y"),
                            info = "Fel: Argumenten i funktionen har felaktiga namn.")
  
  # testar argument
  expect_error(simple_lm(x = 1:10,y = "hej"),"y is not numeric",
               info="Fel: Funktionen ger inte rätt felmeddlende när y inte är numerisk")
  expect_error(simple_lm(x = c(FALSE,TRUE),y = 3:39),"x is not numeric",
               info="Fel: Funktionen ger inte rätt felmeddlende när x inte är numerisk")
  expect_error(simple_lm(x = 1:20,y = 1:3),"not the same number of elements in x and y",
               info="Fel: Funktionen ger inte rätt felmeddlende när x och y är olika långa")
  
  
  # expect_error(simple_lm()(1:4),
  #              info="Fel: Funktionen avbryter/stoppar inte om x har jämt antal element.")
  # 
  # expect_error(simple_lm()(1:4),"x not of odd length.",
  #              info="Fel: Funktionen ger inte rätt felmeddlende när x har jämt antal element.")
  # 
  # expect_true(is.matrix(simple_lm()(1:3)),
  #             info="Fel: Funktionen returnerar inte en matrix")
  # 
  # expect_equal(dim(simple_lm()(1:5)), c(3,3),
  #              info="Fel: Funktionen returnerar en matrix med fel dimension")
  # 
  # expect_equal(dim(simple_lm()(1)), c(1,1),
  #              info="Fel: Funktionen returnerar en matrix med fel dimension för simple_lm()(x=1)")
  # 
  # expect_equal(simple_lm()(1:9)[3:4,5], c(3,2),
  #              info="Fel: Funktionen returnerar en felaktig matris med simple_lm()(1:9)")
  # 
  # expect_equal(simple_lm()(1:11)[6,6], c(1),
  #              info="Fel: Funktionen returnerar en felaktig matris med simple_lm()(1:11)")
  # 
  # expect_equal(simple_lm()(1:11)[1,6], c(6),
  #              info="Fel: Funktionen returnerar en felaktig matris med simple_lm()(1:11)")
  # 
  # expect_equal(simple_lm()(1:11)[6,1], c(11),
  #              info="Fel: Funktionen returnerar en felaktig matris med simple_lm()(1:11)")
  # 
  # expect_equal(simple_lm()(1:5), test1,
  #              info="Fel: Funktionen returnerar en felaktig matris med simple_lm()(1:5)")
  # expect_equal(simple_lm()(c("a","b","c")), test2,
  #              info="Fel: Funktionen returnerar en felaktig matris med simple_lm()(c(\"a\",\"b\",\"c\"))")
  # expect_equal(simple_lm()(c(11,22,33,44,22,33,44)), test3,
  #              info="Fel: Funktionen returnerar en felaktig matris med simple_lm()(c(11,22,33,44,22,33,44))")
  # expect_equal(simple_lm()(x = c(1:7,2:7)), test4,
  #              info="Fel: Funktionen returnerar en felaktig matris med simple_lm()(x=c(1:7,2:7))")
  # 
  # expect_function_code(object = simple_lm(), expected = "return", 
  #                      info = "Fel: return() saknas i funktionen.") 
  # 
  # expect_false(body_contain(object = simple_lm(),expected = "toeplitz"),
  #              info = "Fel: Funktionen toeplitz() används i funktionen.")
  # 
  # expect_false(body_contain(object = simple_lm(),expected = "toeplitz.spam"),
  #              info = "Fel: Funktionen toeplitz.spam() används i funktionen.")
  # 
  # expect_false(body_contain(object = simple_lm(),expected = "Toeplitz"),
  #              info = "Fel: Funktionen Toeplitz() används i funktionen.")
  # 
  # expect_false(body_contain(object = simple_lm(),expected = "spam"),
  #              info = "Fel: Funktioner ur paketet spam används i funktionen.")
  # 
  # expect_false(body_contain(object = simple_lm(),expected = "ts"),
  #              info = "Fel: Funktioner ur paketet ts används i funktionen.")
  # 
  # expect_false(body_contain(object = simple_lm(),expected = "pracma"),
  #              info = "Fel: Funktioner ur paketet pracma används i funktionen.")
  # 
  # expect_silent(object = simple_lm()(x=c(1,2,3)))
  
})
