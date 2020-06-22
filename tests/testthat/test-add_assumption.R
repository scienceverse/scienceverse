test_that("ugh", {
  s <- study() %>%
    add_hypothesis("H1", "Speed and distance will be positively correlated") %>%
    add_analysis("speed_dist_cor",code = cor.test(cars$speed, cars$dist)) %>%
    add_criterion("sig_cor", "p.value", "<", 0.05, "H1", "speed_dist_cor") %>%
    add_criterion("pos_cor", "estimate", ">", 0, "H1", "speed_dist_cor") %>%
    add_eval("corroboration", "Significant positive correlation", "sig_cor & pos_cor", hypothesis_id = "H1") %>%
    add_eval("falsification", "Significant negative correlation", "sig_cor & !pos_cor", hypothesis_id = "H1") %>%
    add_assumption("normality", "Distance and speed will be normally distributed", "H1") %>%
    add_analysis("normal_speed", code = shapiro.test(cars$speed)) %>%
    add_criterion("dist_speed", "p.value", ">", 0.05,
                  hypothesis_id = "normality",
                  analysis_id = "normal_speed") %>%
    add_analysis("normal_dist", code = shapiro.test(cars$dist)) %>%
    add_criterion("dist_norm", "p.value", ">", 0.05,
                  hypothesis_id = "normality",
                  analysis_id = "normal_dist") %>%
    add_eval("corroboration", "Both speed and distance are not significantly non-normal", "dist_speed & dist_norm", hypothesis_id = "normality") %>%
    add_eval("falsification", "Speed and/or distance are significantly non-normal", "!dist_speed | !dist_norm", hypothesis_id = "normality") %>%
    study_analyse()

})

