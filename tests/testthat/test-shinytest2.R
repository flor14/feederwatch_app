library(shinytest2)

<<<<<<< HEAD
test_that("{shinytest2} recording: provincetab_eucdov", {
  app <- AppDriver$new(variant = platform_variant(), name = "provincetab_eucdov", 
      height = 495, width = 425)
  app$set_window_size(width = 979, height = 637)
  app$set_inputs(navbar = "by Province")
  app$set_inputs(species2 = "eucdov")
  app$set_inputs(provinces = c("CA-AB", "CA-MB", "CA-SK"))
=======
test_that("{shinytest2} recording: comrav_by-province", {
  app <- AppDriver$new(variant = platform_variant(), name = "comrav_by-province", 
      height = 743, width = 788)
  app$set_inputs(navbar = "by Province")
  app$set_inputs(species2 = "comrav")
  app$set_inputs(provinces = c("CA-AB", "CA-NB", "CA-NS", "CA-BC", "CA-ON", "CA-QC", 
      "CA-SK"))
  app$set_inputs(provinces = c("CA-AB", "CA-NB", "CA-BC", "CA-ON", "CA-QC", "CA-SK"))
  app$expect_values()
  app$expect_screenshot()
})


test_that("{shinytest2} recording: diversity_byregion", {
  app <- AppDriver$new(variant = platform_variant(), name = "diversity_byregion", 
      height = 743, width = 788)
  app$set_inputs(navbar = "by Province")
  app$set_inputs(navbar = "Diversity")
  app$set_inputs(radio = "region_sub")
  app$expect_values()
  app$expect_screenshot()
})


test_that("{shinytest2} recording: norfli_dataexploration", {
  app <- AppDriver$new(variant = platform_variant(), name = "norfli_dataexploration", 
      height = 788, width = 788)
  app$set_inputs(species = "norfli")
  app$set_inputs(daterange = c("2020-11-28", "2021-04-29"))
  app$set_inputs(daterange = c("2020-11-28", "2021-02-16"))
>>>>>>> 94db30963405d182e0ece7da8cb325ee6355c56a
  app$expect_values()
  app$expect_screenshot()
})
