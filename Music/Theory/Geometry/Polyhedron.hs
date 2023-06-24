module Music.Theory.Geometry.Polyhedron where

-- * Uniform

-- | (n,wythoff-symbol,name,dual)
--   <https://mathworld.wolfram.com/UniformPolyhedron.html>
uniform_polyhedron_tbl :: [(Int, String, String, String)]
uniform_polyhedron_tbl =
  [(1,"3|2 3","tetrahedron","tetrahedron")
  ,(2,"2 3|3","truncated tetrahedron","triakis tetrahedron")
  ,(3,"3/2 3|3","octahemioctahedron","octahemioctacron")
  ,(4,"3/2 3|2","tetrahemihexahedron","tetrahemihexacron")
  ,(5,"4|2 3","octahedron","cube")
  ,(6,"3|2 4","cube","octahedron")
  ,(7,"2|3 4","cuboctahedron","rhombic dodecahedron")
  ,(8,"2 4|3","truncated octahedron","tetrakis hexahedron")
  ,(9,"2 3|4","truncated cube","small triakis octahedron")
  ,(10,"3 4|2","small rhombicuboctahedron","deltoidal icositetrahedron")
  ,(11,"2 3 4|","great rhombicuboctahedron","disdyakis dodecahedron")
  ,(12,"|2 3 4","snub cube","pentagonal icositetrahedron")
  ,(13,"3/2 4|4","small cubicuboctahedron","small hexacronic icositetrahedron")
  ,(14,"3 4|4/3","great cubicuboctahedron","great hexacronic icositetrahedron")
  ,(15,"4/3 4|3","cubohemioctahedron","hexahemioctacron")
  ,(16,"4/3 3 4|","cubitruncated cuboctahedron","tetradyakis hexahedron")
  ,(17,"3/2 4|2","uniform great rhombicuboctahedron","great deltoidal icositetrahedron")
  ,(18,"3/2 2 4|","small rhombihexahedron","small rhombihexacron")
  ,(19,"2 3|4/3","stellated truncated hexahedron","great triakis octahedron")
  ,(20,"4/3 2 3|","great truncated cuboctahedron","great disdyakis dodecahedron")
  ,(21,"4/3 3/2 2|","great rhombihexahedron","great rhombihexacron")
  ,(22,"5|2 3","icosahedron","dodecahedron")
  ,(23,"3|2 5","dodecahedron","icosahedron")
  ,(24,"2|3 5","icosidodecahedron","rhombic triacontahedron")
  ,(25,"2 5|3","truncated icosahedron","pentakis dodecahedron")
  ,(26,"2 3|5","truncated dodecahedron","triakis icosahedron")
  ,(27,"3 5|2","small rhombicosidodecahedron","deltoidal hexecontahedron")
  ,(28,"2 3 5|","great rhombicosidodecahedron","disdyakis triacontahedron")
  ,(29,"|2 3 5","snub dodecahedron","pentagonal hexecontahedron")
  ,(30,"3|5/2 3","small ditrigonal icosidodecahedron","small triambic icosahedron")
  ,(31,"5/2 3|3","small icosicosidodecahedron","small icosacronic hexecontahedron")
  ,(32,"|5/2 3 3","small snub icosicosidodecahedron","small hexagonal hexecontahedron")
  ,(33,"3/2 5|5","small dodecicosidodecahedron","small dodecacronic hexecontahedron")
  ,(34,"5|2 5/2","small stellated dodecahedron","great dodecahedron")
  ,(35,"5/2|2 5","great dodecahedron","small stellated dodecahedron")
  ,(36,"2|5/2 5","dodecadodecahedron","medial rhombic triacontahedron")
  ,(37,"2 5/2|5","truncated great dodecahedron","small stellapentakis dodecahedron")
  ,(38,"5/2 5|2","rhombidodecadodecahedron","medial deltoidal hexecontahedron")
  ,(39,"2 5/2 5|","small rhombidodecahedron","small rhombidodecacron")
  ,(40,"|2 5/2 5","snub dodecadodecahedron","medial pentagonal hexecontahedron")
  ,(41,"3|5/3 5","ditrigonal dodecadodecahedron","medial triambic icosahedron")
  ,(42,"3 5|5/3","great ditrigonal dodecicosidodecahedron","great ditrigonal dodecacronic hexecontahedron")
  ,(43,"5/3 3|5","small ditrigonal dodecicosidodecahedron","small ditrigonal dodecacronic hexecontahedron")
  ,(44,"5/3 5|3","icosidodecadodecahedron","medial icosacronic hexecontahedron")
  ,(45,"5/3 3 5|","icositruncated dodecadodecahedron","tridyakis icosahedron")
  ,(46,"|5/3 3 5","snub icosidodecadodecahedron","medial hexagonal hexecontahedron")
  ,(47,"3/2|3 5","great ditrigonal icosidodecahedron","great triambic icosahedron")
  ,(48,"3/2 5|3","great icosicosidodecahedron","great icosacronic hexecontahedron")
  ,(49,"3/2 3|5","small icosihemidodecahedron","small icosihemidodecacron")
  ,(50,"3/2 3 5|","small dodecicosahedron","small dodecicosacron")
  ,(51,"5/4 5|5","small dodecahemidodecahedron","small dodecahemidodecacron")
  ,(52,"3|2 5/2","great stellated dodecahedron","great icosahedron")
  ,(53,"5/2|2 3","great icosahedron","great stellated dodecahedron")
  ,(54,"2|5/2 3","great icosidodecahedron","great rhombic triacontahedron")
  ,(55,"2 5/2|3","great truncated icosahedron","great stellapentakis dodecahedron")
  ,(56,"2 5/2 3|","rhombicosahedron","rhombicosacron")
  ,(57,"|2 5/2 3","great snub icosidodecahedron","great pentagonal hexecontahedron")
  ,(58,"2 5|5/3","small stellated truncated dodecahedron","great pentakis dodecahedron")
  ,(59,"5/3 2 5|","truncated dodecadodecahedron","medial disdyakis triacontahedron")
  ,(60,"|5/3 2 5","inverted snub dodecadodecahedron","medial inverted pentagonal hexecontahedron")
  ,(61,"5/2 3|5/3","great dodecicosidodecahedron","great dodecacronic hexecontahedron")
  ,(62,"5/3 5/2|3","small dodecahemicosahedron","small dodecahemicosacron")
  ,(63,"5/3 5/2 3|","great dodecicosahedron","great dodecicosacron")
  ,(64,"|5/3 5/2 3","great snub dodecicosidodecahedron","great hexagonal hexecontahedron")
  ,(65,"5/4 5|3","great dodecahemicosahedron","great dodecahemicosacron")
  ,(66,"2 3|5/3","great stellated truncated dodecahedron","great triakis icosahedron")
  ,(67,"5/3 3|2","uniform great rhombicosidodecahedron","great deltoidal hexecontahedron")
  ,(68,"5/3 2 3|","great truncated icosidodecahedron","great disdyakis triacontahedron")
  ,(69,"|5/3 2 3","great inverted snub icosidodecahedron","great inverted pentagonal hexecontahedron")
  ,(70,"5/3 5/2|5/3","great dodecahemidodecahedron","great dodecahemidodecacron")
  ,(71,"3/2 3|5/3","great icosihemidodecahedron","great icosihemidodecacron")
  ,(72,"|3/2 3/2 5/2","small retrosnub icosicosidodecahedron","small hexagrammic hexecontahedron")
  ,(73,"3/2 5/3 2|","great rhombidodecahedron","great rhombidodecacron")
  ,(74,"|3/2 5/3 2","great retrosnub icosidodecahedron","great pentagrammic hexecontahedron")
  ,(75,"|3/2 5/3 35/2","great dirhombicosidodecahedron","great dirhombicosidodecacron")
  ,(76,"2 5|2","pentagonal prism","pentagonal dipyramid")
  ,(77,"|2 2 5","pentagonal antiprism","pentagonal trapezohedron")
  ,(78,"2 5/2|2","pentagrammic prism","pentagrammic dipyramid")
  ,(79,"|2 2 5/2","pentagrammic antiprism","pentagrammic deltohedron")
  ,(80,"|2 2 5/3","pentagrammic crossed antiprism","pentagrammic concave deltohedron")]

-- * Johnson

-- | <https://mathworld.wolfram.com/JohnsonSolid.html>
johnson_names_tbl :: [(Int,String)]
johnson_names_tbl =
  [(1,"Square pyramid")
  ,(2,"Pentagonal pyramid")
  ,(3,"Triangular cupola")
  ,(4,"Square cupola")
  ,(5,"Pentagonal cupola")
  ,(6,"Pentagonal rotunda")
  ,(7,"Elongated triangular pyramid")
  ,(8,"Elongated square pyramid")
  ,(9,"Elongated pentagonal pyramid")
  ,(10,"Gyroelongated square pyramid")
  ,(11,"Gyroelongated pentagonal pyramid")
  ,(12,"Triangular dipyramid")
  ,(13,"Pentagonal dipyramid")
  ,(14,"Elongated triangular dipyramid")
  ,(15,"Elongated square dipyramid")
  ,(16,"Elongated pentagonal dipyramid")
  ,(17,"Gyroelongated square dipyramid")
  ,(18,"Elongated triangular cupola")
  ,(19,"Elongated square cupola")
  ,(20,"Elongated pentagonal cupola")
  ,(21,"Elongated pentagonal rotunda")
  ,(22,"Gyroelongated triangular cupola")
  ,(23,"Gyroelongated square cupola")
  ,(24,"Gyroelongated pentagonal cupola")
  ,(25,"Gyroelongated pentagonal rotunda")
  ,(26,"Gyrobifastigium")
  ,(27,"Triangular orthobicupola")
  ,(28,"Square orthobicupola")
  ,(29,"Square gyrobicupola")
  ,(30,"Pentagonal orthobicupola")
  ,(31,"Pentagonal gyrobicupola")
  ,(32,"Pentagonal orthocupolarotunda")
  ,(33,"Pentagonal gyrocupolarotunda")
  ,(34,"Pentagonal orthobirotunda")
  ,(35,"Elongated triangular orthobicupola")
  ,(36,"Elongated triangular gyrobicupola")
  ,(37,"Elongated square gyrobicupola")
  ,(38,"Elongated pentagonal orthobicupola")
  ,(39,"Elongated pentagonal gyrobicupola")
  ,(40,"Elongated pentagonal orthocupolarotunda")
  ,(41,"Elongated pentagonal gyrocupolarotunda")
  ,(42,"Elongated pentagonal orthobirotunda")
  ,(43,"Elongated pentagonal gyrobirotunda")
  ,(44,"Gyroelongated triangular bicupola")
  ,(45,"Gyroelongated square bicupola")
  ,(46,"Gyroelongated pentagonal bicupola")
  ,(47,"Gyroelongated pentagonal cupolarotunda")
  ,(48,"Gyroelongated pentagonal birotunda")
  ,(49,"Augmented triangular prism")
  ,(50,"Biaugmented triangular prism")
  ,(51,"Triaugmented triangular prism")
  ,(52,"Augmented pentagonal prism")
  ,(53,"Biaugmented pentagonal prism")
  ,(54,"Augmented hexagonal prism")
  ,(55,"Parabiaugmented hexagonal prism")
  ,(56,"Metabiaugmented hexagonal prism")
  ,(57,"Triaugmented hexagonal prism")
  ,(58,"Augmented dodecahedron")
  ,(59,"Parabiaugmented dodecahedron")
  ,(60,"Metabiaugmented dodecahedron")
  ,(61,"Triaugmented dodecahedron")
  ,(62,"Metabidiminished icosahedron")
  ,(63,"Tridiminished icosahedron")
  ,(64,"Augmented tridiminished icosahedron")
  ,(65,"Augmented truncated tetrahedron")
  ,(66,"Augmented truncated cube")
  ,(67,"Biaugmented truncated cube")
  ,(68,"Augmented truncated dodecahedron")
  ,(69,"Parabiaugmented truncated dodecahedron")
  ,(70,"Metabiaugmented truncated dodecahedron")
  ,(71,"Triaugmented truncated dodecahedron")
  ,(72,"Gyrate rhombicosidodecahedron")
  ,(73,"Parabigyrate rhombicosidodecahedron")
  ,(74,"Metabigyrate rhombicosidodecahedron")
  ,(75,"Trigyrate rhombicosidodecahedron")
  ,(76,"Diminished rhombicosidodecahedron")
  ,(77,"Paragyrate diminished rhombicosidodecahedron")
  ,(78,"Metagyrate diminished rhombicosidodecahedron")
  ,(79,"Bigyrate diminished rhombicosidodecahedron")
  ,(80,"Parabidiminished rhombicosidodecahedron")
  ,(81,"Metabidiminished rhombicosidodecahedron")
  ,(82,"Gyrate bidiminished rhombicosidodecahedron")
  ,(83,"Tridiminished rhombicosidodecahedron")
  ,(84,"Snub disphenoid")
  ,(85,"Snub square antiprism")
  ,(86,"Sphenocorona")
  ,(87,"Augmented sphenocorona")
  ,(88,"Sphenomegacorona")
  ,(89,"Hebesphenomegacorona")
  ,(90,"Disphenocingulum")
  ,(91,"Bilunabirotunda")
  ,(92,"Triangular hebesphenorotunda")]

{- | (J,(3,4,5,6,8,10))

> import Music.Theory.Tuple
> map (fmap t6_sum) johnson_ngon_tbl
-}
johnson_ngon_tbl :: [(Int,(Int,Int,Int,Int,Int,Int))]
johnson_ngon_tbl =
  [(1,(4,1,0,0,0,0))
  ,(2,(5,0,1,0,0,0))
  ,(3,(4,3,0,1,0,0))
  ,(4,(4,5,0,0,1,0))
  ,(5,(5,5,1,0,0,1))
  ,(6,(10,0,6,0,0,1))
  ,(7,(4,3,0,0,0,0))
  ,(8,(4,5,0,0,0,0))
  ,(9,(5,5,1,0,0,0))
  ,(10,(12,1,0,0,0,0))
  ,(11,(15,0,1,0,0,0))
  ,(12,(6,0,0,0,0,0))
  ,(13,(10,0,0,0,0,0))
  ,(14,(6,3,0,0,0,0))
  ,(15,(8,4,0,0,0,0))
  ,(16,(10,5,0,0,0,0))
  ,(17,(16,0,0,0,0,0))
  ,(18,(4,9,0,1,0,0))
  ,(19,(4,13,0,0,1,0))
  ,(20,(5,15,1,0,0,1))
  ,(21,(10,10,6,0,0,1))
  ,(22,(16,3,0,1,0,0))
  ,(23,(20,5,0,0,1,0))
  ,(24,(25,5,1,0,0,1))
  ,(25,(30,0,6,0,0,1))
  ,(26,(4,4,0,0,0,0))
  ,(27,(8,6,0,0,0,0))
  ,(28,(8,10,0,0,0,0))
  ,(29,(8,10,0,0,0,0))
  ,(30,(10,10,2,0,0,0))
  ,(31,(10,10,2,0,0,0))
  ,(32,(15,5,7,0,0,0))
  ,(33,(15,5,7,0,0,0))
  ,(34,(20,0,12,0,0,0))
  ,(35,(8,12,0,0,0,0))
  ,(36,(8,12,0,0,0,0))
  ,(37,(8,18,0,0,0,0))
  ,(38,(10,20,2,0,0,0))
  ,(39,(10,20,2,0,0,0))
  ,(40,(15,15,7,0,0,0))
  ,(41,(15,15,7,0,0,0))
  ,(42,(20,10,12,0,0,0))
  ,(43,(20,10,12,0,0,0))
  ,(44,(20,6,0,0,0,0))
  ,(45,(24,10,0,0,0,0))
  ,(46,(30,10,2,0,0,0))
  ,(47,(35,5,7,0,0,0))
  ,(48,(40,0,12,0,0,0))
  ,(49,(6,2,0,0,0,0))
  ,(50,(10,1,0,0,0,0))
  ,(51,(14,0,0,0,0,0))
  ,(52,(4,4,2,0,0,0))
  ,(53,(8,3,2,0,0,0))
  ,(54,(4,5,0,2,0,0))
  ,(55,(8,4,0,2,0,0))
  ,(56,(8,4,0,2,0,0))
  ,(57,(12,3,0,2,0,0))
  ,(58,(5,0,11,0,0,0))
  ,(59,(10,0,10,0,0,0))
  ,(60,(10,0,10,0,0,0))
  ,(61,(15,0,9,0,0,0))
  ,(62,(10,0,2,0,0,0))
  ,(63,(5,0,3,0,0,0))
  ,(64,(7,0,3,0,0,0))
  ,(65,(8,3,0,3,0,0))
  ,(66,(12,5,0,0,5,0))
  ,(67,(16,10,0,0,4,0))
  ,(68,(25,5,1,0,0,11))
  ,(69,(30,10,2,0,0,10))
  ,(70,(30,10,2,0,0,10))
  ,(71,(35,15,3,0,0,9))
  ,(72,(20,30,12,0,0,0))
  ,(73,(20,30,12,0,0,0))
  ,(74,(20,30,12,0,0,0))
  ,(75,(20,30,12,0,0,0))
  ,(76,(15,25,11,0,0,1))
  ,(77,(15,25,11,0,0,1))
  ,(78,(15,25,11,0,0,1))
  ,(79,(15,25,11,0,0,1))
  ,(80,(10,20,10,0,0,2))
  ,(81,(10,20,10,0,0,2))
  ,(82,(10,20,10,0,0,2))
  ,(83,(5,15,9,0,0,3))
  ,(84,(12,0,0,0,0,0))
  ,(85,(24,2,0,0,0,0))
  ,(86,(12,2,0,0,0,0))
  ,(87,(16,1,0,0,0,0))
  ,(88,(16,2,0,0,0,0))
  ,(89,(18,3,0,0,0,0))
  ,(90,(20,4,0,0,0,0))
  ,(91,(8,2,4,0,0,0))
  ,(92,(13,3,3,1,0,0))]

-- * Polymake

{- | Polymake names for archimedean_solid

>>> length polymake_platonic_solid_nm
5
-}
polymake_platonic_solid_nm :: [String]
polymake_platonic_solid_nm =
  ["tetrahedron"
  ,"cube"
  ,"octahedron"
  ,"dodecahedron"
  ,"icosahedron"]

{- | Polymake names for archimedean_solid

>>> length polymake_archimedean_solid_nm
13
-}
polymake_archimedean_solid_nm :: [String]
polymake_archimedean_solid_nm =
  ["truncated_tetrahedron"
  ,"cuboctahedron"
  ,"truncated_cube"
  ,"truncated_octahedron"
  ,"rhombicuboctahedron"
  ,"truncated_cuboctahedron"
  ,"snub_cube"
  ,"icosidodecahedron"
  ,"truncated_dodecahedron"
  ,"truncated_icosahedron"
  ,"rhombicosidodecahedron"
  ,"truncated_icosidodecahedron"
  ,"snub_dodecahedron"]

{- | Polymake names for catalan_solid

>>> length polymake_catalan_solid_nm
13
-}
polymake_catalan_solid_nm :: [String]
polymake_catalan_solid_nm =
  ["triakis_tetrahedron"
  ,"triakis_octahedron"
  ,"rhombic_dodecahedron"
  ,"tetrakis_hexahedron"
  ,"disdyakis_dodecahedron"
  ,"pentagonal_icositetrahedron"
  ,"pentagonal_hexecontahedron"
  ,"rhombic_triacontahedron"
  ,"triakis_icosahedron"
  ,"deltoidal_icositetrahedron"
  ,"pentakis_dodecahedron"
  ,"deltoidal_hexecontahedron"
  ,"disdyakis_triacontahedron"]

{- | Polymake names for johnson_solid

>>> length polymake_johnson_solid_nm
92
-}
polymake_johnson_solid_nm :: [String]
polymake_johnson_solid_nm =
  ["square_pyramid" -- J1
  ,"pentagonal_pyramid" -- J2
  ,"triangular_cupola" -- J3
  ,"square_cupola" -- J4
  ,"pentagonal_cupola" -- J5
  ,"pentagonal_rotunda" -- J6
  ,"elongated_triangular_pyramid" -- J7
  ,"elongated_square_pyramid" -- J8
  ,"elongated_pentagonal_pyramid" -- J9
  ,"gyroelongated_square_pyramid" -- J10
  ,"gyroelongated_pentagonal_pyramid" -- J11
  ,"triangular_bipyramid" -- J12
  ,"pentagonal_bipyramid" -- J13
  ,"elongated_triangular_bipyramid" -- J14
  ,"elongated_square_bipyramid" -- J15
  ,"elongated_pentagonal_bipyramid" -- J16
  ,"gyroelongated_square_bipyramid" -- J17
  ,"elongated_triangular_cupola" -- J18
  ,"elongated_square_cupola" -- J19
  ,"elongated_pentagonal_cupola" -- J20
  ,"elongated_pentagonal_rotunda" -- J21
  ,"gyroelongated_triangular_cupola" -- J22
  ,"gyroelongated_square_cupola" -- J23
  ,"gyroelongated_pentagonal_cupola" -- J24
  ,"gyroelongated_pentagonal_rotunda" -- J25
  ,"gyrobifastigium" -- J26
  ,"triangular_orthobicupola" -- J27
  ,"square_orthobicupola" -- J28
  ,"square_gyrobicupola" -- J29
  ,"pentagonal_orthobicupola" -- J30
  ,"pentagonal_gyrobicupola" -- J31
  ,"pentagonal_orthocupolarotunda" -- J32
  ,"pentagonal_gyrocupolarotunda" -- J33
  ,"pentagonal_orthobirotunda" -- J32
  ,"elongated_triangular_orthbicupola" -- J35
  ,"elongated_triangular_gyrobicupola" -- J36
  ,"elongated_square_gyrobicupola" -- J37
  ,"elongated_pentagonal_orthobicupola" -- J38
  ,"elongated_pentagonal_gyrobicupola" -- J39
  ,"elongated_pentagonal_orthocupolarotunda" -- J40
  ,"elongated_pentagonal_gyrocupolarotunda" -- J41
  ,"elongated_pentagonal_orthobirotunda" -- J42
  ,"elongated_pentagonal_gyrobirotunda" -- J43
  ,"gyroelongated_triangular_bicupola" -- J44
  ,"elongated_square_bicupola" -- J45
  ,"gyroelongated_pentagonal_bicupola" -- J46
  ,"gyroelongated_pentagonal_cupolarotunda" -- J47
  ,"gyroelongated_pentagonal_birotunda" -- J48
  ,"augmented_triangular_prism" -- J49
  ,"biaugmented_triangular_prism" -- J50
  ,"triaugmented_triangular_prism" -- J51
  ,"augmented_pentagonal_prism" -- J52
  ,"biaugmented_pentagonal_prism" -- J53
  ,"augmented_hexagonal_prism" -- J54
  ,"parabiaugmented_hexagonal_prism" -- J55
  ,"metabiaugmented_hexagonal_prism" -- J56
  ,"triaugmented_hexagonal_prism" -- J57
  ,"augmented_dodecahedron" -- J58
  ,"parabiaugmented_dodecahedron" -- J59
  ,"metabiaugmented_dodecahedron" -- J60
  ,"triaugmented_dodecahedron" -- J61
  ,"metabidiminished_icosahedron" -- J62
  ,"tridiminished_icosahedron" -- J63
  ,"augmented_tridiminished_icosahedron" -- J64
  ,"augmented_truncated_tetrahedron" -- J65
  ,"augmented_truncated_cube" -- J66
  ,"biaugmented_truncated_cube" -- J67
  ,"augmented_truncated_dodecahedron" -- J68
  ,"parabiaugmented_truncated_dodecahedron" -- J69
  ,"metabiaugmented_truncated_dodecahedron" -- J70
  ,"triaugmented_truncated_dodecahedron" -- J71
  ,"gyrate_rhombicosidodecahedron" -- J72
  ,"parabigyrate_rhombicosidodecahedron" -- J73
  ,"metabigyrate_rhombicosidodecahedron" -- J74
  ,"trigyrate_rhombicosidodecahedron" -- J75
  ,"diminished_rhombicosidodecahedron" -- J76
  ,"paragyrate_diminished_rhombicosidodecahedron" -- J77
  ,"metagyrate_diminished_rhombicosidodecahedron" -- J78
  ,"bigyrate_diminished_rhombicosidodecahedron" -- J79
  ,"parabidiminished_rhombicosidodecahedron" -- J80
  ,"metabidiminished_rhombicosidodecahedron" -- J81
  ,"gyrate_bidiminished_rhombicosidodecahedron" -- J82
  ,"triminished_rhombicosidodecahedron" -- J83
  ,"snub_disphenoid" -- J84
  ,"snub_square_antisprim" -- J85
  ,"sphenocorona" -- J86
  ,"augmented_sphenocorona" -- J87
  ,"sphenomegacorona" -- J88
  ,"hebesphenomegacorona" -- J89
  ,"disphenocingulum" -- J90
  ,"bilunabirotunda" -- J91
  ,"triangular_hebesphenorotunda" -- J92
  ]

