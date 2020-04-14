# test if reciprocation to self-morph is higher than to other-morphs
t.test(
  kin$recip_self,
  kin$recip_other,
  paired = TRUE,
  conf.level = 0.975
)
