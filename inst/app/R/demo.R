kin_demo <- function() {
  kin_data <- data.frame(
    trust_self  = c(1,2,2,1,1,1,1,1,2,0,2,0,1,2,2,3,2,2,1,1,2,0,0,1),
    trust_other = c(1,2,2,0,1,0,0,0,1,0,1,0,1,1,1,0,1,2,2,0,0,0,2,1),
    recip_self  = c(0,1,3,2,1,1,1,3,3,2,3,1,1,2,3,3,3,1,1,1,3,0,3,1),
    recip_other = c(1,1,2,2,3,2,1,3,3,1,3,0,1,3,3,3,3,0,3,0,1,0,3,2)
  )

  desc <- list(
    description = list(
      trust_self  = "Number of trusting moves towards self-morphs",
      trust_other = "Number of trusting moves towards self-morphs",
      recip_self  = "Number of reciprocating moves towards other-morphs",
      recip_other = "Number of reciprocating moves towards other-morphs"
    ),
    dataType = rep("int", 4) # all variables are integer types
  )



  s <- study(name = "Kinship and Prosocial Behaviour (Demo)",
             description = "A reanalysis of data from DeBruine (2002) Facial Resemblance Enhances Trust, PRSLB.",
             year = 2020) %>%
    add_author(orcid = "0000-0002-7523-5539",
               surname = "DeBruine",
               given = "Lisa M.",
               roles = c("con", "dat", "sof", "dra", "edi"),
               email = "lisa.debruine@glasgow.ac.uk") %>%
    add_author(orcid = "0000-0002-0247-239X",
               surname = "Lakens",
               given = "Daniël",
               roles = c("con", "ana", "dra", "edi")) %>%
    add_hypothesis(id = "self_pref",
                   description = "Cues of kinship will increase prosocial behaviour. Cues of kinship will be manipulated by morphed facial self-resemblance. Prosocial behaviour will be measured by responses in the trust game. The prediction is that the number of trusting AND/OR reciprocating moves will be greater to self morphs than to other morphs.") %>%
    add_analysis(id = "trust",
                 code = t.test(kin$trust_self,
                               kin$trust_other,
                               paired = TRUE,
                               conf.level = 0.975)) %>%
    add_analysis(id = "recip",
                 code = t.test(kin$recip_self,
                               kin$recip_other,
                               paired = TRUE,
                               conf.level = 0.975)) %>%
    add_criterion("t_lo", "conf.int[1]", ">", 0.0,
                  "trust", "self_pref") %>%
    add_criterion("t_hi", "conf.int[2]", ">", 0.2,
                  "trust") %>%
    add_criterion("r_lo", "conf.int[1]", ">", 0.0,
                  "recip") %>%
    add_criterion("r_hi", "conf.int[2]", ">", 0.2,
                  "recip") %>%
    add_eval("corroboration",
             evaluation = "(t_lo & t_hi) | (r_lo & r_hi)",
             description = "The hypothesis is corroborated if the 97.5% CI lower bound is greater than 0 and the 97.5% CI upper bound is greater than 0.2 (the SESOI) for either the trust or reciprocation moves.") %>%
    add_eval("falsification",
             evaluation = "!t_hi & !r_hi",
             description = "The hypothesis is falsified if the 97.5% CI upper bound is smaller than 0.2 (the SESOI) for both trust and reciprocation.") %>%
    add_data(id = "kin",
             data = kin_data,
             vardesc = desc,
             url = "https://osf.io/ewfhs/") %>%
    study_analyse()

  s
}
