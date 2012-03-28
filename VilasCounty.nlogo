extensions [matrix]
globals [numberDefPatches lessCommon listDbg currGraphPatch RecomputeBA total-harvest bug-kcal bug-smr]
breed[birds bird]
birds-own [bugsEaten bird-energy SMR]
patches-own [
  patchNum row col landcov2001 canopy2001 houses1996 houses2005 frontage 
  lakesize road_length soil ownership zoning tree-size diam-dist distribution-type basal-area
  growth-matrix r Pt k dP poletimber-cords sawtimber-Mbf monetary-value bugs-amt
]

to setup
  ca
  
  file-close
  reset-ticks
  set RecomputeBA true
  set numberDefPatches 0
  set listDbg 0
  set total-harvest 0
  file-open input-file
  let tempreporter file-read-line
  calc-max-patches
  read-input
  initialize-forests
  if (RecomputeBA) [ ask patches with [tree-size > 0] [recompute-basal-area] ]
  recolor-patches
  ask patches [set r 0.2 set Pt 1 set k 5 
    if landcov2001 = 24 [set k 1] ;;high intensity developed
    if landcov2001 = 23 [set k 2] ;;medium developed
    if landcov2001 = 22 or landcov2001 = 12[set k 3] ;;developed low intensity or perennial ice/snow
    if landcov2001 = 31 [set k 1.5] ;;barren land (rock/sand/clay)
    if landcov2001 = 21 [set k 4] ;;developed open space
    if landcov2001 = 51 or landcov2001 = 52 [set k 4.5] ;;shrub
    if landcov2001 >= 71 and landcov2001 <= 82 [set k 7.5] ;;crops and herbacious
    if landcov2001 >= 90 [set k 2.0] ;;woody wetlands and emergent herbaceous wetlands
    ifelse (landcov2001 != 11 and landcov2001 != 255) [set bugs-amt 60 set Pt 2]
    [set Pt 0]
  ]
  create-birds start-birds [move-to one-of patches set size 3.5 set bird-energy 50 set SMR 14.5]
  set bug-kcal 6
  set bug-smr 6
  ;set pt-kcal 900


  set currGraphPatch one-of patches with [tree-size > 0]
  update-plot
end

to step
  set listDbg 0
  recolor-patches
  ;tree-growth
  go
end

to go
  
  if (ticks > 0 and ticks mod 365 = 70) [
    tree-growth
  ]
  ask patches[
    if (landcov2001 != 11 and landcov2001 != 255) [herbaceous-vegetation bugs-step]

  ]
  if (ticks mod recolor-interval = 0) [recolor-patches]
  update-plot
  
  ask birds [birds-step]
  
  ifelse(stopticks)[  ] [tick]
  
  
  
end

to read-input
  let patch-counter 0
  foreach sort patches [
    if patch-counter < numberDefPatches [
      ask ? [
        set patchNum file-read
        set row file-read 
        set col file-read
        set landcov2001 file-read
        set canopy2001 file-read
        set houses1996 file-read
        set houses2005 file-read
        set frontage file-read
        set lakesize file-read
        set road_length file-read
        set soil file-read
        set ownership file-read
        set zoning file-read
        if (landcov2001 = 41 or landcov2001 = 43 or landcov2001 = 42 or landcov2001 = 90)
        [ set pcolor green ]
      ]
      set patch-counter patch-counter + 1
    ]
  ]
end


to calc-max-patches
  while [not file-at-end?]
  [
    let tempthing file-read-line
    set numberDefPatches numberDefPatches + 1
  ]
  file-close
  file-open input-file
  let tempreporter file-read-line
end


to initialize-forests
  ;deciduous
  ask patches with [landcov2001 = 41]
  [
    let temprand random 100
    ifelse temprand < 43 
    [
      set tree-size 10
    ]
    [
      ifelse (temprand >= 43 and temprand < 80) [
        set tree-size 8
      ]
      [
        set tree-size 5 
      ]
    ]
    
    if tree-size = 10
    [
      let temprand2 random 100 
      if temprand2 < 47 [ ]
      if (temprand2 >= 47 and temprand2 < 74) [set tree-size tree-size + 2 ]
      if (temprand2 >= 74 and temprand2 < 86) [set tree-size tree-size + 4 ]
      if (temprand2 >= 86 and temprand2 < 93) [set tree-size tree-size + 6 ]
      if (temprand2 >= 93 and temprand2 < 96) [set tree-size tree-size + 8 ]
      if (temprand2 >= 96 and temprand2 < 97) [set tree-size tree-size + 10 ]
      if (temprand2 > 97) [set tree-size tree-size + 15 ]
    ]
    
  ]
  
  ;coniferous
  ask patches with [landcov2001 = 42]
  [
    let temprand random 100
    ifelse temprand < 56 
    [
      set tree-size 10
    ]
    [
      ifelse (temprand >= 56 and temprand < 82) [
        set tree-size 8
      ]
      [
        set tree-size 5 
      ]
    ]
    
    if tree-size = 10
    [
      let temprand2 random 100 
      if temprand2 < 37 [ ]
      if (temprand2 >= 37 and temprand2 < 59) [set tree-size tree-size + 2 ]
      if (temprand2 >= 59 and temprand2 < 69) [set tree-size tree-size + 4 ]
      if (temprand2 >= 69 and temprand2 < 80) [set tree-size tree-size + 6 ]
      if (temprand2 >= 80 and temprand2 < 86) [set tree-size tree-size + 8 ]
      if (temprand2 >= 86 and temprand2 < 91) [set tree-size tree-size + 10 ]
      if (temprand2 >= 91 and temprand2 < 99) [set tree-size tree-size + 15 ]
      if (temprand2 >= 99) [set tree-size tree-size + 20]
    ]
    
  ]
  
  ;mixed  
  ask patches with [landcov2001 = 43]
  [
    let temprand random 100
    ifelse temprand < 43 [set tree-size 10]
    [
      ifelse (temprand >= 43 and temprand < 80) [set tree-size 8] [
        set tree-size 5 
      ]]
    if tree-size = 10
    [
      let temprand2 random 100 
      if temprand2 < 37 [ ]
      if (temprand2 >= 37 and temprand2 < 60) [set tree-size tree-size + 2 ]
      if (temprand2 >= 60 and temprand2 < 71) [set tree-size tree-size + 4 ]
      if (temprand2 >= 71 and temprand2 < 82) [set tree-size tree-size + 6 ]
      if (temprand2 >= 82 and temprand2 < 88) [set tree-size tree-size + 8 ]
      if (temprand2 >= 88 and temprand2 < 93) [set tree-size tree-size + 10 ]
      if (temprand2 >= 93 and temprand2 < 99) [set tree-size tree-size + 15 ]
      if (temprand2 >= 99) [set tree-size tree-size + 20]
    ]
    
  ]
  
  ;wetland
  ask patches with [landcov2001 = 90]
  [
    let temprand random 100
    ifelse temprand < 43 [
      set tree-size 10
    ]
    [
      ifelse (temprand >= 43 and temprand < 80) [
        set tree-size 8
      ]
      [
        set tree-size 5 
      ]
    ]
    
    if tree-size = 10
    [
      let temprand2 random 100 
      if temprand2 < 37 [ ]
      if (temprand2 >= 37 and temprand2 < 60) [set tree-size tree-size + 2 ]
      if (temprand2 >= 60 and temprand2 < 71) [set tree-size tree-size + 4 ]
      if (temprand2 >= 71 and temprand2 < 82) [set tree-size tree-size + 6 ]
      if (temprand2 >= 82 and temprand2 < 88) [set tree-size tree-size + 8 ]
      if (temprand2 >= 88 and temprand2 < 93) [set tree-size tree-size + 10 ]
      if (temprand2 >= 93 and temprand2 < 99) [set tree-size tree-size + 15 ]
      if (temprand2 >= 99) [set tree-size tree-size + 20]
    ]
    
  ]
  
  
  ;create diameter distribution
  ask patches with [tree-size > 0] [
    let target-basal-area (random 51) + 60
    set distribution-type random 2
    set diam-dist [0 0 0 0 0 0 0 0 0 0 0 0]
    
    ifelse distribution-type = 0 
    ;normal distribution
    [
      let cv 0.559016994 ;=sqrt(5 / (2 * (2 + 5 + 1)))
      let stdev (cv * tree-size)
      while [basal-area < target-basal-area] [
        let temp-tree random-normal tree-size stdev
        if (temp-tree > 0 and temp-tree < 29) [
          let bin-to-put 0
          
          ifelse (temp-tree < 3) [ ] [
            ifelse (temp-tree >= 3 and temp-tree < 5) [set bin-to-put 1 ] [
              ifelse (temp-tree >= 5 and temp-tree < 7) [set bin-to-put 2 ] [
                ifelse (temp-tree >= 7 and temp-tree < 9) [set bin-to-put 3 ] [
                  ifelse (temp-tree >= 9 and temp-tree < 11) [set bin-to-put 4 ] [
                    ifelse (temp-tree >= 11 and temp-tree < 13) [set bin-to-put 5 ] [
                      ifelse (temp-tree >= 13 and temp-tree < 15) [set bin-to-put 6 ] [
                        ifelse (temp-tree >= 15 and temp-tree < 17) [set bin-to-put 7 ] [
                          ifelse (temp-tree >= 17 and temp-tree < 19) [set bin-to-put 8 ] [
                            ifelse (temp-tree >= 19 and temp-tree < 21) [set bin-to-put 9 ] [
                              ifelse (temp-tree >= 21 and temp-tree < 23) [set bin-to-put 10 ] [
                                set bin-to-put 11;
                              ]]]]]]]]]]]
          
          let temp-bin-count (item bin-to-put diam-dist)
          set diam-dist replace-item bin-to-put diam-dist (temp-bin-count + 1)
          set basal-area basal-area + ((0.005454 * ( temp-tree ^ 2 )))
        ]
      ]        
    ]
    ;negative exponential
    [
      let lamda 1 / tree-size
      let iterator-list (n-values 12 [?])
      let fd-vals [0 0 0 0 0 0 0 0 0 0 0 0]
      let normalized-sums [0 0 0 0 0 0 0 0 0 0 0 0]
      foreach iterator-list [
        set fd-vals replace-item ? fd-vals (lamda * e ^ (lamda * (? + 1)))
      ]
      
      let fd-sum (sum fd-vals)
      foreach iterator-list [
        set normalized-sums replace-item ? normalized-sums  ((item ? fd-vals) / fd-sum)
      ]
      while [basal-area < target-basal-area] [
        let rand random-float 1
        let bin-to-put 0
        ifelse (rand < item 0 normalized-sums) [ ] [
          ifelse (rand >= item 1 normalized-sums and rand < item 2 normalized-sums) [set bin-to-put 1] [
            ifelse (rand >= item 2 normalized-sums and rand < item 3 normalized-sums) [set bin-to-put 2] [
              ifelse (rand >= item 3 normalized-sums and rand < item 4 normalized-sums) [set bin-to-put 3] [
                ifelse (rand >= item 4 normalized-sums and rand < item 5 normalized-sums) [set bin-to-put 4] [
                  ifelse (rand >= item 5 normalized-sums and rand < item 6 normalized-sums) [set bin-to-put 5] [
                    ifelse (rand >= item 6 normalized-sums and rand < item 7 normalized-sums) [set bin-to-put 6] [
                      ifelse (rand >= item 7 normalized-sums and rand < item 8 normalized-sums) [set bin-to-put 7] [
                        ifelse (rand >= item 8 normalized-sums and rand < item 9 normalized-sums) [set bin-to-put 8] [
                          ifelse (rand >= item 9 normalized-sums and rand < item 10 normalized-sums) [set bin-to-put 9] [
                            ifelse (rand >= item 10 normalized-sums and rand < item 11 normalized-sums) [set bin-to-put 10] [
                              set bin-to-put 11
                            ]]]]]]]]]]]
        
        let temp-diam ((ln(rand / lamda)) / lamda)
        let temp-bin-count (item bin-to-put diam-dist)
        set diam-dist replace-item bin-to-put diam-dist (temp-bin-count + 1)
        set basal-area basal-area + ((0.005454 * ( (temp-diam ^ 2 ))))
      ]
    ]
    calc-timber-volumes
    calc-monetary-value
    set pcolor 62
  ]
  
end

to recolor-patches
  if patchColoring = "forest type" [
    color-forest-type
  ]
  
  if patchColoring = "land coverage" [
    color-land-coverage
  ]
  
  if patchColoring = "canopy size" [
    color-canopy-size
  ]
  
  if patchColoring = "mean tree size" [
    color-mean-tree-size 
  ]
  
  if patchColoring = "basal area" [
    color-basal-area
  ]
  
  if patchColoring = "herbacious vegitation" [
    color-vegitation
  ]
  
  if patchColoring = "monetary value" [
    color-monetary-value 
  ]
  
  if patchColoring = "bugs" [
   color-bugs 
  ]
  
end

to color-bugs
  let max-bugs [bugs-amt] of max-one-of patches [bugs-amt]
  ask patches [
    ifelse (bugs-amt = 0) [ set pcolor black ] [
      set pcolor scale-color red bugs-amt max-bugs 1
    ]
  ]
end

to color-forest-type
  ask patches [
    ifelse landcov2001 = 41 [
      set pcolor lime
    ]
    [ 
      ifelse landcov2001 = 42 [
        set pcolor green
      ]
      [
        ifelse landcov2001 = 43 [
          set pcolor turquoise 
        ]
        [
          ifelse landcov2001 = 90 [
            set pcolor cyan
          ]
          [set pcolor black ]
        ]
      ]
    ] 
  ]
end

to color-monetary-value
  let max-val [monetary-value] of max-one-of patches [monetary-value]
  let min-val [monetary-value] of min-one-of (patches with [monetary-value > 0]) [monetary-value]
  ask patches [
    ifelse (tree-size = 0) [ set pcolor black ] [set pcolor scale-color green monetary-value max-val min-val]
  ]
end

to color-land-coverage
  
  ;ask patches [set pcolor white]
  ask patches [
    if landcov2001 = 41 [
      set pcolor 67
    ]
    if landcov2001 = 42 [
      set pcolor 53
    ]
    if landcov2001 = 43 [
      set pcolor 44
    ]
    if landcov2001 = 90 [
      set pcolor 97
    ]
    if landcov2001 = 11 [
      set pcolor blue 
    ]
    if landcov2001 = 21 [
      set pcolor 137.5
    ]
    if landcov2001 = 95 [
      set pcolor 95 
    ]
    if landcov2001 = 81 [
      set pcolor 45 
    ]
    if landcov2001 = 71 [
      set pcolor 29 
    ]
    if landcov2001 = 23 [
      set pcolor 15 
    ]
    if landcov2001 = 52 [
      set pcolor 37 
    ]
    if landcov2001 = 22 [
      set pcolor 17
    ]
    if landcov2001 = 31 [
      set pcolor 38 
    ]
    if landcov2001 = 82 [
      set pcolor 36
    ]
    if landcov2001 = 255 [
      set pcolor black
    ]
    
  ]
end

to color-canopy-size
  ask patches with [tree-size > 0] [set pcolor scale-color green canopy2001 100 0]
  ask patches with [tree-size = 0] [set pcolor black]
end

to color-mean-tree-size
  ask patches [
    ifelse (tree-size = 0) [set pcolor black]
    [
      let iterator-list (n-values 12 [?])
      let mean-size 0
      foreach iterator-list [
        set mean-size mean-size + (2 * (? + 1) * (item ? diam-dist))
      ]
      set mean-size (mean-size) / (sum diam-dist)
      set mean-size (mean-size - 2) / 22 ;normalized mean
      if (mean-size > .8 ) [set mean-size .8]
      if (mean-size < .05 ) [set mean-size .05]
      
      set pcolor (50 + (10 * (1 - mean-size))) 
    ]
    
  ]
  ask patches with [tree-size > 0]
  [
    
  ]
end

to color-basal-area
  ask patches[
    ifelse (tree-size = 0) [set pcolor black] [
      let temp-ba ((basal-area - 60) / 110)
      if (temp-ba > .8 ) [set temp-ba .8]
      if (temp-ba < .15 ) [set temp-ba .15]
      set pcolor (50 + (10 * (1 - temp-ba)))
    ]
  ]
end

to color-vegitation
  ask patches[
    ifelse (Pt = 0) [ set pcolor black ] [
     ; let temp-pt (Pt / 7)
      ;if (temp-pt > .8 ) [set temp-pt .8]
      ;if (temp-pt < .15 ) [set temp-pt .15]
      set pcolor scale-color green Pt 4 0
    ]
  ]
end

to tree-growth
  let iterator-list (n-values 12 [?])
  set listDbg 0
  ask patches with [tree-size > 0][
    create-growth-matrix
    let formatted-diams [[0]]
    set formatted-diams replace-item 0 formatted-diams diam-dist
    let diams matrix:from-row-list formatted-diams
    set diams matrix:transpose diams
    let newDiams matrix:times growth-matrix diams
    set newDiams matrix:transpose newDiams
    set newDiams matrix:to-row-list newDiams
    set newDiams item 0 newDiams
    
    let c1 0
    let c2 0
    ifelse(landcov2001 = 41)[set c1 18.187 set c2 .097] [
      ifelse(landcov2001 = 42)[set c1 7.622 set c2 .059] [
        set c1 4.603 set c2 .035
      ]]
    if((c1 - c2 * basal-area) > 0)[
      set newDiams replace-item 0 newDiams ((item 0 newDiams) + (c1 - c2 * basal-area)) 
    ]
    
    
    ;foreach iterator-list [
    ; set newDiams replace-item ? newDiams (round item ? newDiams)
    ;]
    
    set diam-dist newDiams 
    recompute-basal-area 
    calc-timber-volumes
    calc-monetary-value
  ]
end

to-report upgrowth [growPatch bin-to-grow]
  
  let category [landcov2001] of growPatch
  let return-value 0
  ifelse (category = 41) [ 
    set return-value (.0164 - .00001 * ([basal-area] of growPatch) + .0055 * bin-to-grow - 0.0002 * (bin-to-grow ^ 2))
  ] [ifelse (category = 42) [ 
    set return-value (.0069 - .00001 * ([basal-area] of growPatch) + .0059 * bin-to-grow - 0.0002 * (bin-to-grow ^ 2))
  ]
  [
    set return-value (.0134 - .00002 * ([basal-area] of growPatch) + .0051 * bin-to-grow - 0.0002 * (bin-to-grow ^ 2) + ((.00002 * 80) * bin-to-grow))
  ]]
  
  report return-value
  
end

to-report mortality [growPatch bin-to-grow]
  let category [landcov2001] of growPatch
  let return-value 0
  ifelse (category = 41) [ 
    set return-value .0336 - .0018 * (bin-to-grow) + .0001 * (bin-to-grow ^ 2) - ((.00002 * 80) * bin-to-grow)
  ] [ifelse (category = 42) [ 
    set return-value .0418 - .0009 * (bin-to-grow)
  ]
  [
    set return-value .0417 - .0033 * (bin-to-grow) + .0001 * (bin-to-grow ^ 2)
  ]]
  
  report return-value
end

to-report ingrowth [growPatch]
  let category [landcov2001] of growPatch
  let ret-val 0
  ifelse (category = 41) [set ret-val 18.187 - .097 * [basal-area] of growPatch ] [
    ifelse (category = 42) [set ret-val 7.622 - .059 * [basal-area] of growPatch ] [
      set ret-val 4.603 - .035 * [basal-area] of growPatch
    ]]
  report ret-val
end

to create-growth-matrix
  let iterator-list (n-values 12 [?])
  let tempSurv (1 - mortality self 2)
  let tempUpg (1 - upgrowth self 2)
  let tempInsert (tempSurv * tempUpg)
  let matrixList [ ]
  let tempLine [[0 0 0 0 0 0 0 0 0 0 0 0]]
  set tempLine (replace-item 0 tempLine (replace-item 0 (item 0 tempLine)  tempInsert))
  set matrixList tempLine
  
  let tempCounter 0
  
  foreach (n-values 10 [?]) [
    let thisbin ((? + 1) * 2)
    let binabove ((? + 2) * 2)
    set tempLine [[0 0 0 0 0 0 0 0 0 0 0 0]]
    let survival1 (1 - mortality self thisbin)
    let upgrowth1 (upgrowth self thisbin)
    let survival2 (1 - mortality self binabove)
    let upgrowth2 (1 - upgrowth self binabove)
    set tempLine (replace-item 0 tempLine (replace-item (?) (item 0 tempLine) ( survival1 * upgrowth1))) 
    set tempLine (replace-item 0 templine (replace-item (? + 1) (item 0 tempLine) (survival2 * upgrowth2)))
    set matrixList sentence matrixList tempLine
    set tempCounter tempCounter + 1
  ]
  set tempLine [[0 0 0 0 0 0 0 0 0 0 0 0]]
  set tempLine (replace-item 0 tempLine (replace-item 11 (item 0 tempLine) (1 - mortality self 24)))
  set matrixList sentence matrixList tempLine
  set growth-matrix matrix:from-row-list matrixList   
end

to recompute-basal-area
  let iterator-list (n-values 12 [?])
  let temp-basal-area 0
  foreach iterator-list [
    set temp-basal-area temp-basal-area + ((.005454 * ( 2 * (? + 1)) ^ 2) * (item ? diam-dist));
  ]
  set basal-area temp-basal-area
end

to herbaceous-vegetation
  
  ifelse energetics [
    ;if (ticks mod 365) = 1 [
     ; set Pt (2) 
    ;]
    if (ticks mod 365) < 175[ ;;ticks mod 365 
      set Pt (2 + r * Pt * (1 - (Pt / K))) 
    ]
    
    if (ticks mod 365) >= 175 [
      set Pt (2 - (r / 2) * Pt) 
    ]
    ;set Pt (Pt + dP)
  ]
  [
    if (ticks mod 365) = 1 [
      set Pt (Pt + 1) 
    ]
    if (ticks mod 365) < 175[ ;;ticks mod 365 
      set dP (r * Pt * (1 - (Pt / K)) - (bugs-amt * (bugs-eating-rate / 1000))) 
    ]
    
    if (ticks mod 365) >= 175 [
      set dP (-(r / 2) * Pt) - (bugs-amt * bugs-eating-rate * Pt) ]
    set Pt (Pt + dP)
  ]
  
  
  if tree-herb-interact [set Pt Pt * ((k * (1 - e ^ (-0.34 * canopy2001)) ^ .5) / k)]
  
  if (Pt < 0) [set Pt 0]
  
end

to bugs-step
  ifelse energetics [
    let old-amt bugs-amt
    ifelse ((bugs-amt * bug-kcal) < (pt-kcal * .1 * Pt)) [
      if (ticks mod 20 = 0) [
        set bugs-amt (bugs-amt + ((pt-kcal * .1 * Pt) / (2 * bug-kcal)))
      ]
      set Pt (Pt - (((Pt * pt-kcal * .1) - (bugs-amt * bug-kcal)) / pt-kcal))
    ]
    [
      set bugs-amt (bugs-amt * bug-kcal - pt-kcal * .1 * Pt) / bug-kcal
      set Pt 0
    ]
  ]
  [
    set bugs-amt ((bugs-amt * ( bugs-eating-rate / 10 )  * Pt) - (bugs-amt * (1 / bug-death-rate)))
  ]
  
 ; if (Pt < 0) [set Pt 0]
  ;if (bugs-amt <= 0) [ set bugs-amt 1 ]
  
  
end

to update-plot
  set-current-plot "Tree Sizes" 
  set-plot-x-range 0 5
  set-plot-y-range 0 5
  set-histogram-num-bars 12
  let temp-dist [0 0 0 0 0 0 0 0 0 0 0 0]
  clear-plot
  ask currGraphPatch [
    foreach (n-values 12 [?]) [
      set temp-dist replace-item ? temp-dist (round item ? diam-dist)
      plotxy ? (item ? [diam-dist] of currGraphPatch)
    ]
  ]
  

  set-current-plot "Harvest Value per Year"
  plot harvest-per-year
  
  set-current-plot "Energy on Patch"
  if (Energy-bar-or-line) [clear-plot]
  set-current-plot-pen "plant-energy"
  ifelse (Energy-bar-or-line) [set-plot-pen-mode 1] [set-plot-pen-mode 0]
  let plant-en ([Pt] of currGraphPatch * [pt-kcal] of currGraphPatch)
   ifelse (Energy-bar-or-line) [plotxy 0 plant-en * .01] [plot plant-en * .01]
  set-current-plot-pen "bug-energy"
  ifelse (Energy-bar-or-line) [set-plot-pen-mode 1] [set-plot-pen-mode 0]
  ifelse (Energy-bar-or-line) [plotxy 1 ([bugs-amt] of currGraphPatch) * .1] [plot ([bugs-amt] of currGraphPatch) * .1]
  if any? birds-on currGraphPatch [
    let tempen 0
    foreach sort birds-on currGraphPatch [
     set tempen tempen + [bird-energy] of ? 
    ]
    set-current-plot-pen "bird-energy"
   ifelse (Energy-bar-or-line) [set-plot-pen-mode 1] [set-plot-pen-mode 0]  
    
    ifelse (Energy-bar-or-line) [plotxy 2 tempen] [plot tempen]
  ]
   
  
end

to newGraphPatch
  set currGraphPatch one-of patches with [tree-size > 0]
end

to calc-timber-volumes
  let iterator-list (n-values 12 [?])
  let tempc1 0
  let tempc2 0
  let tempc3 0
  let tempc4 0
  let tempc5 0
  let tempc6 0
  let tempc7 0
  ifelse (landcov2001 = 41) [ 
    set tempc1 5.34
    set tempc2 -0.23
    set tempc3 1.15
    set tempc4 0.54
    set tempc5 0.83
    set tempc6 0.06
  ] [
  ifelse (landcov2001 = 42) [ 
    set tempc1 7.19
    set tempc2 -0.28
    set tempc3 1.44
    set tempc4 0.39
    set tempc5 0.83
    set tempc6 0.11
  ] [
  set tempc1 6.43
  set tempc2 -0.24
  set tempc3 1.34
  set tempc4 0.47
  set tempc5 0.73
  set tempc6 0.08
  ]]
  
  let polevol 0
  let sawvol 0
  let vol 0
  let isHardwood false
  foreach iterator-list [
    let thisdiam (? + 1) * 2
    if (thisdiam >= 6) [
      ifelse (landcov2001 = 42) [
        ifelse (thisdiam <= 8) [set tempc7 4] [set tempc7 9]
        set isHardwood true
      ]
      [
        ifelse (thisdiam <= 10) [set tempc7 4] [set tempc7 9]
      ]
      
      let H 4.5 + (tempc1 * (1 - e ^ (tempc2 * thisdiam)) ^ tempc3) * (80 ^ tempc4) * ((1.00001 - (tempc7 / thisdiam)) ^ tempc5) * (basal-area ^ tempc6)
      ifelse ( landcov2001 = 41 ) [ set vol (2.706 + (.002 * thisdiam ^ 2) * H ) ] [
        ifelse (landcov2001 = 42 ) [ set vol (1.375 + (.002 * thisdiam ^ 2) * H ) ] [
          set vol ((.002 * thisdiam ^ 2) * H ) 
        ]]
      ifelse (tempc7 = 4) [set polevol polevol + vol] [set sawvol sawvol + (scribner-conversion vol thisdiam isHardwood)]
    ]
  ]
  set poletimber-cords (polevol / 128)
  set sawtimber-Mbf ((sawvol * 12) / 1000)
end

to calc-monetary-value
  ifelse (landcov2001 = 41) [set monetary-value ((14 * poletimber-cords) + (147 * sawtimber-Mbf)) ] [
    ifelse (landcov2001 = 42) [set monetary-value ((12 * poletimber-cords) + (151 * sawtimber-Mbf))  ] [
        set monetary-value ((13 * poletimber-cords) + (127 * sawtimber-Mbf)) 
    ]]
end

to-report scribner-conversion [vol diameter isHardwood]
  let softList [0.783 0.829 0.858 0.878 0.895 0.908 0.917 0.924 0.930 0.932 0.936]
  let hardList [0.832 0.861 0.883 0.900 0.913 0.924 0.933 0.940 0.945 0.954 ]
  let retVal 0
  let listVal 0

  ifelse (isHardwood) [
    set listVal ((diameter / 2) - 5)
    set retVal (vol * item listVal hardList)
  ]
  [
    set listVal ((diameter / 2 ) - 4)
    set retVal (vol * item listVal softList)
  ]
  
  report retVal
end

to select-new-histogram-patch
  if mouse-inside? [
    if mouse-down? [
      if ([tree-size] of (patch mouse-xcor mouse-ycor) > 0) [ 
        set currGraphPatch patch mouse-xcor mouse-ycor
      ]
    ]
  ] 
end


to harvest-trees
  ifelse (harvest-type = "clear cut") [  
    if mouse-inside? [
      if mouse-down? [ 
        ask patch mouse-xcor mouse-ycor[
          set diam-dist [0 0 0 0 0 0 0 0 0 0 0 0]
          set basal-area 0
          set pcolor white
        ]
      ]
    ]
  ]
  [
    ifelse (harvest-type = "diameter limit") 
    [
      if mouse-inside? [
        if mouse-down? [ 
          if ([tree-size] of patch mouse-xcor mouse-ycor > 0)[
            ask patch mouse-xcor mouse-ycor[
              set pcolor white
              diameter-limit-cut
            ]
          ]
        ] 
      ]
    ]
    [
      ifelse (harvest-type = "Arbogast")[
        if mouse-inside? [
          if mouse-down? [
            if ([tree-size] of patch mouse-xcor mouse-ycor > 0)[ 
              ask patch mouse-xcor mouse-ycor[
                set pcolor white
                arbogast-cut
              ]
            ]
          ]
        ] 
      ] [ ]
    ]
  ]
      
end

to arbogast-cut
  let dmin 2
  let dmax 24
  let nmax 0
  let to-keep-list [0 0 0 0 0 0 0 0 0 0 0 0]
  
  let tempsum 0
 
  foreach (n-values 12 [?]) [
    let di ((? + 1) * 2)
    let bai (.005454 * di ^ 2)
    set tempsum (tempsum + ((bai * arbogast-q) * ((dmax - di) / 2)))
  ]
  set nmax arbogast-B / tempsum

  set to-keep-list replace-item (11) to-keep-list nmax
  
  foreach (n-values 11 [?]) [
    set to-keep-list replace-item (10 - ?) to-keep-list (nmax * arbogast-q ^ (? + 1))
  ]
  
  foreach (n-values 12 [?]) [
   if (item ? diam-dist > item ? to-keep-list) [
     set diam-dist replace-item ? diam-dist (item ? to-keep-list)
   ]
  ]
  
  
end

to diameter-limit-cut ;;cut stuff ABOVE limit
    let i 11
    while [diameter-limit < ((i + 1) * 2)] [
      set diam-dist replace-item i diam-dist 0 ;; replace-item index list value
      set i (i - 1)
    ]
  
end

to auto-harvest
  if (ticks mod harvest-frequency = 0) [
    ask n-of patches-to-harvest patches with [tree-size > 0] [
      ifelse (harvest-type = "diameter limit") [
        diameter-limit-cut
      ][ifelse (harvest-type  = "clear cut") [
        set diam-dist [0 0 0 0 0 0 0 0 0 0 0 0]
        set basal-area 0
      ][if (harvest-type = "Arbogast") [
          arbogast-cut
        ]
      ]]
      set pcolor white 
      let oldVal monetary-value
      calc-timber-volumes
      calc-monetary-value
      set total-harvest total-harvest + (oldVal - monetary-value)
    ]
  ]
  
end

to-report harvest-per-year
  ifelse ticks > 365 [
    report total-harvest / (floor (ticks / 365))
  ]
  [
    report total-harvest
  ]
end

to birds-step
  
  rt (random 100) - 50
  fd 1
  let bugs-eaten-this-tick 0
  
  ifelse ([bugs-amt] of patch-here >= bugs-eaten-per-tick) [
    set bugs-eaten-this-tick bugs-eaten-per-tick
  ]
  [
    set bugs-eaten-this-tick bugs-amt
  ]
  set bugsEaten bugsEaten + bugs-eaten-this-tick
    
  ask patch-here [ if bugs-amt > 0 
    [set bugs-amt bugs-amt - bugs-eaten-per-tick] ]
  
  if(bird-labels) [ set label round bugsEaten ]
  
  if(bird-reprod) [ 
    set bird-energy bird-energy - SMR
    ifelse (bird-energy <= 0) [ die ]
    [
      set bird-energy bird-energy + ( bugs-eaten-this-tick)
      if bird-energy >= (birds-energy-to-reproduce * SMR) [
        set bird-energy (bird-energy / 2)
        hatch 1 [set bugs-amt 0]
      ]
    ]
  ]
  
end
  
  
  
  
  
  
@#$#@#$#@
GRAPHICS-WINDOW
357
10
849
523
50
50
4.7723
1
10
1
1
1
0
1
1
1
-50
50
-50
50
0
0
1
ticks

BUTTON
10
59
73
92
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
84
58
147
91
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
9
103
72
136
NIL
step
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

CHOOSER
9
201
176
246
patchColoring
patchColoring
"land coverage" "canopy size" "forest type" "mean tree size" "basal area" "herbacious vegitation" "monetary value" "bugs"
7

INPUTBOX
1
369
246
429
input-file
vilasChunk.txt
1
0
String

SLIDER
8
158
180
191
recolor-interval
recolor-interval
1
5000
26
1
1
NIL
HORIZONTAL

PLOT
18
437
218
587
Tree Sizes
NIL
NIL
0.0
12.0
0.0
5.0
true
false
PENS
"default" 1.0 1 -16777216 true

BUTTON
230
442
320
475
New Patch
newGraphPatch
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
79
102
187
135
NIL
harvest-trees
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

CHOOSER
193
94
309
139
harvest-type
harvest-type
"clear cut" "diameter limit" "Arbogast"
2

BUTTON
225
483
325
516
Select Patch
select-new-histogram-patch
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

SWITCH
197
58
300
91
record
record
0
1
-1000

SLIDER
182
162
354
195
arbogast-B
arbogast-B
60
140
69
1
1
NIL
HORIZONTAL

SLIDER
182
198
351
231
arbogast-q
arbogast-q
1.1
2.0
1.1
0.1
1
NIL
HORIZONTAL

SLIDER
182
238
354
271
diameter-limit
diameter-limit
2
24
10
2
1
NIL
HORIZONTAL

SWITCH
197
22
301
55
stopticks
stopticks
1
1
-1000

BUTTON
1057
28
1160
61
auto harvest
auto-harvest
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
1037
74
1209
107
harvest-frequency
harvest-frequency
1
1000
147
1
1
NIL
HORIZONTAL

SWITCH
1177
30
1332
63
area-or-dispersed
area-or-dispersed
1
1
-1000

SLIDER
1039
114
1211
147
patches-to-harvest
patches-to-harvest
1
4000
350
1
1
NIL
HORIZONTAL

MONITOR
1041
173
1145
218
total harvest ($)
total-harvest
17
1
11

PLOT
1056
235
1256
385
Harvest Value per Year
NIL
NIL
0.0
10.0
0.0
10.0
true
false
PENS
"default" 1.0 0 -16777216 true

MONITOR
1160
176
1269
221
NIL
harvest-per-year
17
1
11

SLIDER
6
248
178
281
bugs-eating-rate
bugs-eating-rate
1
100
6
1
1
NIL
HORIZONTAL

SLIDER
6
283
178
316
bug-death-rate
bug-death-rate
1
150
100
1
1
NIL
HORIZONTAL

SLIDER
181
278
353
311
start-birds
start-birds
1
100
100
1
1
NIL
HORIZONTAL

SLIDER
180
316
352
349
bugs-eaten-per-tick
bugs-eaten-per-tick
0
100
80
1
1
NIL
HORIZONTAL

SWITCH
248
369
349
402
bird-labels
bird-labels
1
1
-1000

SWITCH
248
405
352
438
bird-reprod
bird-reprod
1
1
-1000

SWITCH
1061
397
1217
430
tree-herb-interact
tree-herb-interact
0
1
-1000

SLIDER
6
319
177
352
birds-energy-to-reproduce
birds-energy-to-reproduce
2
100
20
1
1
NIL
HORIZONTAL

SWITCH
1061
440
1208
473
energetics
energetics
0
1
-1000

SLIDER
861
114
1033
147
pt-kcal
pt-kcal
100
10000
10000
10
1
NIL
HORIZONTAL

PLOT
851
235
1051
385
Energy on Patch
NIL
Energy
0.0
10.0
0.0
10.0
true
false
PENS
"plant-energy" 1.0 1 -10899396 true
"bird-energy" 1.0 1 -16777216 true
"bug-energy" 1.0 1 -2674135 true

SWITCH
871
403
1028
436
Energy-bar-or-line
Energy-bar-or-line
0
1
-1000

@#$#@#$#@
WHAT IS IT?
-----------
This section could give a general understanding of what the model is trying to show or explain.


HOW IT WORKS
------------
This section could explain what rules the agents use to create the overall behavior of the model.


HOW TO USE IT
-------------
This section could explain how to use the model, including a description of each of the items in the interface tab.


THINGS TO NOTICE
----------------
This section could give some ideas of things for the user to notice while running the model.


THINGS TO TRY
-------------
This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.


EXTENDING THE MODEL
-------------------
This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.


NETLOGO FEATURES
----------------
This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.


RELATED MODELS
--------------
This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.


CREDITS AND REFERENCES
----------------------
This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 4.1.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
