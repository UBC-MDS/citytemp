library(shinytest2)

test_that("{shinytest2} recording: trend_nyc_4_10_temp", {
  app <- AppDriver$new(variant = platform_variant(), name = "trend_nyc_4_10_temp", 
      height = 654, width = 931)
  app$set_inputs(month_range = c(4, 12))
  app$set_inputs(month_range = c(4, 10))
  app$set_inputs(state = "NY")
  app$set_inputs(city = "New York City")
  app$expect_values()
  app$expect_screenshot()
})


test_that("{shinytest2} recording: rank_pa_high_june", {
  app <- AppDriver$new(variant = platform_variant(), name = "rank_pa_high_june", 
      height = 654, width = 931)
  app$set_inputs(tabs = "city_ranking")
  app$set_inputs(statename = "PA")
  app$set_inputs(month = "June")
  app$expect_values()
  app$expect_screenshot()
})
