library(shinytest2)

test_that("{shinytest2} recording: provincetab_eucdov", {
  app <- AppDriver$new(variant = platform_variant(), name = "provincetab_eucdov", 
      height = 495, width = 425)
  app$set_window_size(width = 979, height = 637)
  app$set_inputs(navbar = "by Province")
  app$set_inputs(species2 = "eucdov")
  app$set_inputs(provinces = c("CA-AB", "CA-MB", "CA-SK"))
  app$expect_values()
  app$expect_screenshot()
})
