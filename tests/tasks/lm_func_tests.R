context("lm_func()")

test_that("lm_func", {  
  
  
  body_contain<-function(object,expected) {any(grepl(x = as.character(body(object)), pattern = expected))}
  package_loaded<-function(object){any(grepl(object, search()))}
  
  
  
  expect_true(exists("lm_func"),
              info = "Fel: lm_func() saknas.")
  
  expect_true(is.function(lm_func),
              info = "Fel: lm_func() är inte en funktion.")
  
  expect_function_self_contained(object = lm_func,
                                 "Fel: Funktionen innehåller fria variabler = definerade utanför funktionen")
  
  expect_function_arguments(object = lm_func,expected = c("X","y","beta_test","mu0"),
                            info = "Fel: Argumenten i funktionen har felaktiga namn.")
  
  # testar argument
  # testar y:
  expect_error(lm_func(X = trees[,1:2],y = data.frame(3),beta_test = FALSE,mu0 = c(0,0,0)) ,
               info="Funktionen ger inte fel när y inte är en vektor")
  expect_error(lm_func(X = trees[,1:2],y = data.frame(3),beta_test = FALSE,mu0 = c(0,0,0)) ,
               "^y is not a vector$",
               info="Fel: Funktionen ger inte rätt felmeddlende när y inte är en vektor: y is not a vector")
  
  # testar X:
  expect_error(lm_func(X = list(2,"h"),y = trees$Volume,beta_test = FALSE,mu0 = c(0,0,0)),
               info="Funktionen ger inte fel när X inte är en matris eller en data.frame")
  expect_error(lm_func(X = list(2,"h"),y = trees$Volume,beta_test = FALSE,mu0 = c(0,0,0)),
               "^X is not a matrix or a data.frame$",
               info="Fel: Funktionen ger inte rätt felmeddlende när x inte är en matris eller en data.frame: X is not a matrix or a data.frame")
  
  # testar längden på mu0:
  expect_error(lm_func(X = trees[,1:2],y = trees$Volume,beta_test = FALSE,mu0 = 1:10),
               info="Funktionen ger inte fel när mu0 inte har rätt längd")
  expect_error(lm_func(X = trees[,1:2],y = trees$Volume,beta_test = FALSE,mu0 = 1:10),
               "^mu0 does not have the correct number of elements$",
               info="Fel: Funktionen ger inte rätt felmeddlende när  mu0 inte har rätt längd: mu0 does not have the correct number of elements")
  
  # testar output
  expect_true(is.list(lm_func(X = trees[,1,drop=FALSE],y = trees$Volume,beta_test = FALSE,mu0 = c(0,0)) ),
              info="Fel: Funktionen returnerar inte en lista")
  
  
  expect_true( length(lm_func(X = trees[,1,drop=FALSE],y = trees$Volume,beta_test = FALSE,mu0 = c(0,0)))==3,
               info="Fel: Funktionen returnerar inte en lista av rätt längd när beta_test = FALSE")
  
  expect_true( length(lm_func(X = trees[,1,drop=FALSE],y = trees$Volume,beta_test = TRUE,mu0 = c(0,0)))==4,
               info="Fel: Funktionen returnerar inte en lista av rätt längd när beta_test = TRUE")
  
  expect_equal(names(lm_func(X = trees[,1,drop=FALSE],y = trees$Volume,beta_test = FALSE,mu0 = c(0,0)) ),c("beta","sigma","beta_cov"),
               info="Fel: Funktionen returnerar inte en lista med rätt namn på elementen när beta_test = FALSE")
  
  expect_equal(names(lm_func(X = trees[,1,drop=FALSE],y = trees$Volume,beta_test = TRUE,mu0 = c(0,0)) ),c("beta","sigma","beta_cov","test_mat"),
               info="Fel: Funktionen returnerar inte en lista med rätt namn på elementen när beta_test = TRUE")
  
  
  # kollar förbjudna funktioner:
  expect_false(body_contain(object = lm_func,expected = "lm"),
               info = "Fel: Funktionen lm() används i funktionen.")
  expect_false(body_contain(object = lm_func,expected = "glm"),
               info = "Fel: Funktionen glm() används i funktionen.")
  expect_false(body_contain(object = lm_func,expected = "summary.lm"),
               info = "Fel: Funktionen summary.lm() används i funktionen.")
  expect_false(body_contain(object = lm_func,expected = "summary"),
               info = "Fel: Funktionen summary() används i funktionen.")
  expect_false(body_contain(object = lm_func,expected = "vcov"),
               info = "Fel: Funktionen vcov() används i funktionen.")
  
  expect_false(package_loaded(object = "vcov"),
               info = "Fel: Paketet vcov används i funktionen.")
  
  
  
  
})
