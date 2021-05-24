from GeneratorInterface.HiGenCommon.highMultiplicityGenFilter_cfi import *

highMultiplicityGenFilter_HydjetCent030 = highMultiplicityGenFilter.clone(
  nMin = cms.untracked.int32(2200)
)

highMultiplicityGenFilterSequence_HydjetCent30100 = cms.Sequence(~highMultiplicityGenFilter_HydjetCent030)
