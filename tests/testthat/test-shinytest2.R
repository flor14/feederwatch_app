library(shinytest2)

test_that("{shinytest2} recording: haiwoo_province", {
  app <- AppDriver$new(name = "haiwoo_province", height = 789, width = 1169)
  app$set_inputs(navbar = "by Province")
  app$set_inputs(species2 = "haiwoo")
  app$expect_values()
})


test_that("{shinytest2} recording: region", {
  app <- AppDriver$new(variant = platform_variant(), name = "region", height = 789, 
      width = 1169)
  app$set_inputs(navbar = "Diversity")
  app$set_inputs(radio = "region_sub")
  app$expect_screenshot()
})


test_that("{shinytest2} recording: whtspa", {
  app <- AppDriver$new(variant = platform_variant(), name = "whtspa", height = 744, 
      width = 1169)
  app$set_inputs(navbar = "by Province")
  app$set_inputs(species2 = "whtspa")
  app$set_inputs(provinces = c("CA-ON", "CA-QC", "CA-AB", "CA-BC", "CA-MB", "CA-NB", 
      "CA-NS"))
  app$set_inputs(provinces = c("CA-ON", "CA-QC", "CA-AB", "CA-BC", "CA-NB", "CA-NS"))
  app$expect_screenshot()
})


test_that("{shinytest2} recording: region-lala", {
  app <- AppDriver$new(variant = platform_variant(), name = "region-lala", height = 789, 
      width = 1169)
  app$set_inputs(navbar = "by Province")
  app$set_inputs(radio = "region_sub")
  app$expect_screenshot()
  app$expect_values()
})
