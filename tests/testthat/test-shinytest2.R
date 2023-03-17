library(shinytest2)

test_that("{shinytest2} recording: rank_pa_high_june", {
  app <- AppDriver$new(variant = platform_variant(), name = "rank_pa_high_june", 
      height = 654, width = 931)
  app$set_inputs(tabs = "city_ranking")
  app$set_inputs(statename = "PA")
  app$set_inputs(month = "June")
  app$expect_values()
  app$expect_screenshot()
})


test_that("{shinytest2} recording: trand_seattle_temp_6_11", {
  app <- AppDriver$new(variant = platform_variant(), name = "trand_seattle_temp_6_11", 
      height = 654, width = 931)
  app$set_inputs(state = "WA")
  app$set_inputs(month_range = c(6, 12))
  app$set_inputs(month_range = c(6, 11))
  app$expect_values()
  app$expect_screenshot()
})


test_that("{shinytest2} recording: trend_nyc_1_10", {
  app <- AppDriver$new(variant = platform_variant(), name = "trend_nyc_1_10", height = 654, 
      width = 931)
  app$set_inputs(state = "NY")
  app$set_inputs(city = "New York City")
  app$set_inputs(data_type = "Precipitation")
  app$set_inputs(month_range = c(1, 10))
  app$expect_values()
  app$expect_screenshot()
})
