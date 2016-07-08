'lo.index@mergeNodes'(G9912, G9913, G9914):-
  G9912='lo.index#trNode'(G9916, G9917, G9918, G9919), 
  G9929='lo.index#trNode'(G9924, G9925, G9926, G9927), 
  'lo.index@HashLen'(G9932), 
  'lo.index@commonMaskLen'(G9916, G9924, G9932, G9937), 
  'lo.arith@min'(G9937, G9917, G9941), 
  'lo.arith@min'(G9941, G9925, G9945), 
  G9947=G9945, 
  'lo.index@commonMask'(G9916, G9947, G9952), 
  G9954=G9952, !, 
  'lo.index@condExp70'(G9914, G9925, G9959, G9960, G9961, G9927, G9912, G9926, G9924, G9966, G9967, G9968, G9916, G9954, G9918, G9919, G9913, G9974, G9975, G9976, G9917, G9947).