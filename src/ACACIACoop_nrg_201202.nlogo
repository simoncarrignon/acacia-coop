globals[
  
 ; screen coordinat  
  xleft
  xright
  ytop
  ybottom
;------------------
  
  angle-step  ;
  distance-step  
  max-h ;90degre..
     
  count-sims;num de la sim
  count-steps ;current step of the sim
  title
  
  dec ;;
  distA ;;
  
;-------------------------
;  Counter for outputs (plot & file)
  num-cooperation
  num-defection
  num-adaptation-agent;
  num-adaptation-obstacle
  num-agents-reach-fruit
  num-adaptation-agent-rich
  num-adaptation-agent-explorer
  p-richa-a
  p-expla-a
  v-space
;-------------------------

;-------------------------
; colors 
  cwall
  cobstacle
  cfruit
  ccross-fruit
  cmin-atfruit
  cmax-atfruit
;-------------------------
  
  
  agent-color ; colors must correspond to agent possible states
;-------------------------
;possible state
  s-explore         ;the agent explore the world
  s-adapt-fruit     ;the agent has seen a fruit
  s-adapt-rich      ;the agent has seen a rich agent 
  s-adapt-explorer  ;the agent has seen an over agent exploring the world
  s-adapt-obstacle  ;the agent has seen an obstacle
  s-leave-fruit     ;the agent is neighbourust leaving a fruit
 
  s-poor            ;enregy's agent is less than 30%
  s-rich            ;enregy's agent is more than 70%
  s-normal          ;enregy's agent is more than 70%
  s-dead

;-------------------------
  rep               ;number of selfish wanted at initialisation
  curRep            ;current number of selfish
]

turtles-own 
[
  go-x       ; coordinates of entity seen
  go-y

  code                 ;utiliser pour coder les entitées vues et à quel distance/angle
  mycode
  last-turn            ;used when turning around an obstacle
  last-counter         ;
  
  state                ;integer coding for the state of the turtle : s-explore, s-adapt, etc..
  e-state              ;integer coding for enregy of the turtle : s-rich, s-poor, etc..
  
  angle-code-interdit  ;used to know where the agent cannot look (wall, obstacle)
  energy               ;store the energy level of each agent
  sentiment            ;store the feeling of the agent : if 0 => defect ; else cooperate
  strategy             ;store the strategy of the agent : 0 selfish, 1 realistic, 2 kind
  experience           ;code l'Experience d'une maniere A COMPRENDRE
  last-seen            ;verifie que l'agent questionner ne vient pas déjà d'être interogé
  selfish-flag         ;utiliser dans la defection TRES BIZARRE
  last-coop            ;used by tit for tatter to know what was the previous cooperation result
  entitySeen           ;code of the entiy seen : 1 for fruit, 2 or 3 for agent, 
  neighbour            ;one of the nearest agent if there is an agent near. Nobody, else
    
  s-see-rich           ;true if the agent have seen a fruit
  s-see-fruit          ;true if the agent have seen a rich agent
]

patches-own
[
  fruit-level
]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Procédure principale appelée lors de l'appuie sur START
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to move

  ca  ;clear all
  set-screen-coors ;redéfini le nom des var de l'Ecran

  set count-sims 0  ;initialisation du decompte des sims
  file-open "../data/" + date-and-time + "ACACIAout.csv"   ;

  show-global-header ;affiche l'en tête du fichier qui sera imprimé
    

;bypass manual settings----------
;let counter_type 0
;repeat 2[
set fruitPerTree 5
repeat 3 [
set num-obstacles 0
repeat 5 [
set generating-speed 3
repeat 3 [
;
;set counter_type 0
;set rep 100
;
;repeat  21 [
;--------------------------------

repeat num-sims [
     set curRep 0
     clear-turtles
     
     clear-patches

     setup-general-params
     setup-world
     
     set count-sims count-sims + 1
     set count-steps 0
     set num-agents-reach-fruit 0
     set num-adaptation-agent 0
     set num-adaptation-agent-rich 0
     set num-adaptation-agent-explorer 0
     set num-adaptation-obstacle 0
    
    ;si:Decembre 2010 : if pour faire séries de mesures


;    if( counter_type = 0)[ set Distribution-Agents "selfish" ]
;    if( counter_type = 1)[ set Distribution-Agents "kind" ]
;    if( counter_type = 2)[ set Distribution-Agents "1/3kind1/3realist1/3selfish" ]

  
     ask turtles [setup-memory] 
          set v-space (( count patches - count patches with [pcolor = cobstacle or pcolor = cwall] )   / count patches ) ;calul du pourcentage de patch libres
     
     ifelse global-output = true[ show-global-results ][ show-case-results ]
      
      ;modified to do the good number of sim
     repeat num-steps
     [ 
       if play = false [stop]      
       if ( count-steps > 0 and count-steps mod (2 * 2 ^ (6 - generating-speed)) = 0 ) [create-custom-turtles 1 [setup-fruit]]
      
       ;---------------------------
       ask turtles [ move-agent ]  
       ;---------------------------
       
       set count-steps count-steps + 1
       ifelse(global-output = true)[ show-global-results ][ show-case-results ]
  
     ]
  ]
  
  
;--------------------------------
;        set rep rep - 5
;   ]    
;   set counter_type counter_type + 1
;  ]
   
   set generating-speed generating-speed + 1
   ]
  set num-obstacles num-obstacles + 50  
  ]
  
   set fruitPerTree fruitPerTree + 6
  
 ]
; 
;  ;set big-memory not big-memory
; 
;  ]
  
  ;old export way
 ;export-output date-and-time + "ACACIAout_" + count-sims + ".csv"   ;
    
    file-close
;--------------------------------
end 

;-------------------------------------------------------------------------
; Put the turtle into the "w" state and change its color into the associated color
; turtle context
to change-state [w]
   set state w
   set color item w agent-color
  ; ifelse last-turn != -1 [][ set last-turn -1 ]
end
;-------------------------------------------------------------------



;-------------------------------------------------------------------
;-------------------------------------------------------------------
;-------------------------------------------------------------------
;-------------------------------------------------------------------
;      DEPLACEMENT OF THE AGENTS
;-------------------------------------------------------------------
; this function implents how the agents walk and how they
; react when they face an obstacle
to walk

  
ifelse pcolor-of patch-ahead 1 = cwall or pcolor-of patch-ahead 1 = cobstacle
[ wiggle][fd 1]

end
to wiggle
change-state s-explore
lt random 360
end
;-------------------------------------------------------------------------------------





;----------------------------------------------------------------------------------------
;  Decode Functions
;----------------------------------------------------------------------------------------
; Return angle given angle-code
to-report decode-angle [angle-code]
  report (0 - attention-angle / 2) + angle-step * (angle-code)
end
;----------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------
; Return distance given distance-code
to-report decode-distance [distance-code]
  report distance-step * distance-code
end
;----------------------------------------------------------------------------------------


;----------------------------------------------------------------------------------------
;========================  COOP  =======================================================
;----------------------------------------------------------------------------------------

;le 17 Décembre 2010: ajout de la non cooperation en dessous d'un certain seuil d'energie
to coop
   let real-coop true
   let i who ;the current turtle number
   let survive 2 / 3 ;limite à partir de laquel l'agent ne coopère plus.
;   if real-coop = true[
;     
;     T > R > P > S
;     
;     let gaini 0
;     let gainj 0
;     
;     if(strategy = 0 and strategy-of neighbour = 0)[ set gaini P set gainj P]
;     if(strategy = 0 and strategy-of neighbour = 2)[ set gaini T set gainj S] 
;   
;     if(strategy = 2 and strategy-of neighbour = 2)[ set gaini D set gainj T] 
;     if(strategy = 2 and strategy-of neighbour = 1)[ ifelse(last-coop-of neighbour = "D")[ set gaini = P set gainj = P][ set gaini = S set gainj = T]] 
;
;    
;   ]
    
   if ( (energy-of turtle i > ( survive * total-energy) ) and (energy-of turtle i < energy-of neighbour) and (last-seen != neighbour) )[ 
   ;if the energie of the neighbour we ask to cooperate is sup and we have not asked to the other just before

        ;defection
        if (strategy-of neighbour = 0) [defection i who-of neighbour ]
        if (strategy-of neighbour = 1) [ ;TFT
          let l-c last-coop
          if(big-memory)[ set l-c item who-of neighbour last-coop ] ;if big-memory enable, check what was the previous comportement of j
          ifelse( l-c = "D") [ defection i who-of neighbour ][cooperation i who-of neighbour]] 
          
        ;cooperation
        if (strategy-of neighbour = 2) [cooperation i who-of neighbour]
        set last-seen neighbour
  ]
  ;show last-coop
end

to cooperation [i j]
    locals [sum-energy]

    set sum-energy energy + energy-of turtle j
    set energy int (sum-energy / 2)
    set energy-of turtle j int (sum-energy / 2)
    
    set num-cooperation num-cooperation + 1
    ;Modifié
    if memory = true
    [ifelse experience < 0 [set experience 1] [set experience experience + 1]
    
     if experience = 3 and strategy != 2 [set strategy strategy + 1 change-shape]]
     ifelse(big-memory)[ set last-coop replace-item j last-coop "C" ][ set last-coop "C" ]
    
end
to defection [i j]

    set num-defection num-defection + 1   
    if memory = true [
      ifelse experience > 0 [set experience -1] [set experience experience - 1]
      if experience = -3 and strategy != 0 [set strategy strategy - 1 change-shape]
    ]
  ifelse(big-memory)[ set last-coop replace-item j last-coop "D" ][ set last-coop "D" ]

end

;----------------------------------------------------------------------------------------
;========================  /COOP  =======================================================
;----------------------------------------------------------------------------------------

;;Function used to select how are shaped agents
to change-shape
ifelse(adaptCol)[
  if strategy = 1 [set shape "butterfly_real"]
  if strategy = 0 [set shape "butterfly_self"]
  if strategy = 2 [set shape "butterfly_kind"]
][
  if strategy = 1 [set shape "butterfly_real1"]
  if strategy = 0 [set shape "butterfly_self1"]
  if strategy = 2 [set shape "butterfly_kind1"]
]
end

to e-update 
  set energy energy - 1
  if (energy > int (total-energy * 3 / 4))[set e-state s-rich set label "R" set label-color blue]
  if (energy <= int (total-energy * 3 / 4)) and (energy >= int (total-energy / 4))[set e-state s-normal set label "N" set label-color green]
  if energy < int (total-energy / 4) and energy > 4 [set e-state s-poor set label "P" set label-color red]
  if energy < 4  [set hidden?  true set label "" set e-state s-dead];pq temporiser?
  if energy <= 0 [die]
end 
  
;----------------------------------------------------------------------------------------
; Move agent: Here is modelized the global behaviour of the agents
to move-agent
  
  e-update
  look
  let union go-to ;the core behavior is her
  walk
  
end

;----------------------------------------------------------------------------------------
;Here we change the heading of the Agent depending on what is seen and how it is expect to adapt
;return a code giving what the agent have done ie go to a fruit, 
to-report go-to
 if ( entitySeen = 1 ) [ go-fruit set s-see-fruit true report 0]
 if ( ( entitySeen = 2 or entitySeen = 3 ) and is-agent? neighbour and who-of neighbour != who) [if ( agent-coop ) [ coop ] if(state != s-adapt-fruit)[let adpt agentAdapt report 1]]
 if ( entitySeen = 4 and obs-a ) [ go-obs report 2]
 change-state s-explore report -1
end
;----------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------
; Set agent's heading to the location choose depending on adaption 

to set-heading
  if (not ( xcor = go-x or ycor = go-y) ) [
    ifelse torus = false
    [ set heading towardsxy-nowrap go-x go-y ]
    [ set heading towardsxy go-x go-y ]
  ]
end
;-------------------------------------------------------
;-------------------------------------------------------
;-------------------------------------------------------

;-------------------------------------------------------
; Function Look :
; 
to look
locals[distance-code angle-code]

  set angle-code-interdit[]
  set angle-step attention-angle / attention-resolution 
  set distance-step attention-distance / attention-resolution 

  ; attention-resolution is always assumed to be an even number
  set mycode 10000
  set distance-code 0
  let cpt 0

  ; at each distance step we loop for each angle step (in proc scan-line). 
  ;If a fruit is find we stop the scan
  while [distance-code < attention-resolution and int(mycode / 100) != 1]     [
    scan-line distance-code
    set distance-code distance-code + 1
  ]

  ifelse int (mycode / 100) < 5[
     set entitySeen int (mycode / 100) 
     set distance-code int( (mycode - entitySeen * 100) / 10 )
     set angle-code int(mycode - entitySeen * 100 - distance-code * 10) 
     let dist decode-distance distance-code
     let angle decode-angle angle-code
     set go-x int( pxcor-of patch-right-and-ahead angle dist) 
     set go-y int( pycor-of patch-right-and-ahead angle dist)     
  
  ][set entitySeen 6] 
end




;----------------------------------------------------------------------------------------
;      SCAN-LINE :
; Scan along one neighborhood line, and encode entities found.
to scan-line [distance-code] 
  locals [angle see-what dist angle-code xc yc]
  set see-what 5
  set dist (decode-distance distance-code) 
  set angle-code  0
  set xc 0 set yc 0

  repeat attention-resolution  ;
  [ 
  if (member? angle-code angle-code-interdit) = false; si l'angle n'est pas dans la liste des angles interdit
    [
     set angle decode-angle angle-code ; change the angle code to an angle depending of the heading of agent
     set xc get-x dist angle
     set yc get-y dist angle
     if (valid xc yc) = true ; if coordinate are ok (ie not outbound, we check them)
     [ 
        let ncode 0
        let en 5
        set see-what pcolor-of patch-right-and-ahead angle dist;      
        ;my strange strategie to see how the agent looks at its world
;           set pcolor-of patch-right-and-ahead angle dist white;
;                 wait .005
;          set pcolor-of patch-right-and-ahead angle dist see-what;
;                 
        if see-what = cwall or see-what = cobstacle  [ set en 4 set angle-code-interdit sentence angle-code angle-code-interdit ]; ajouter angle auquel on voit obstacle � la liste des angles "interdits"
        if empty? values-from (turtles-on patch-right-and-ahead angle dist) [who] = false 
        [    
          let a turtle first values-from (turtles-on patch-right-and-ahead angle dist) [who]
          if who-of a != who and (energy-of a > 4) [set see-what color-of a set neighbour a ifelse energy-of neighbour > total-energy - 10 [ set en 2 ][ set en 3 ] ]
        ]
 
        if see-what = cfruit [ set en 1 ]       
        set ncode 100 * en + 10 * distance-code + angle-code ;encode what we see 
        if ncode <= mycode [ set mycode ncode ] 
     ]
   ]
   set angle-code angle-code + 1

 ] 

end


;----------------------------------------------------------------------------------------
;proc go-obs :
; when an agent see an obstacle, it directs its heading toward the obstacle until it is near the obstacle
; when the agent is near enough, it choose randomly a direction, right or left, and start to circle the obstacle 
to go-obs

  set-heading   
  if ( distancexy-nowrap  go-x go-y) <= distA and ( distancexy-nowrap  go-x go-y) > 1
  [      
     ifelse (last-turn < 0) [ ifelse (random 2) = 0  [ lt dec set last-turn 0 ][ rt dec set last-turn 1 ] ]
                            [ ifelse ( last-turn = 0 )[ lt dec ] [ rt dec ] ]
  ]
  change-state s-adapt-obstacle
end

;----------------------------------------------------------------------------------------
to-report agentAdapt

  let  hto heading-of neighbour ;store the neighbourg heading in hto

  if( richa-a and entitySeen = 2 ) [set heading ( hto + 180) change-state s-adapt-rich report s-adapt-rich]
  if( expla-a and entitySeen = 3 and state != s-adapt-rich and hto >= heading - head-tolerance and hto <= heading + head-tolerance  ) [ set heading hto change-state s-adapt-explorer report s-adapt-explorer]

  report -1
end


;----------------------------------------------------------------------------------------
;go-fruit:
; If agent is at fruit (or almost), decrease fruit-level and keep the
; agent there, else go to fruit
to go-fruit
  locals [tl ntl cl]
  set tl 0 
  set ntl 0 
  set cl 0
 
    ; verify if fruit at which the agent is aimed is not exhausted
   ; if it is, revert agent's state to exploring
   ifelse (fruit-level-of patch go-x go-y) > 0 
   [
    ifelse  pcolor = cfruit or (distancexy-nowrap  go-x go-y) <= 1
    [
         ; decrease fruit level depending on fruitPerTree
       ;set tl 100 * exp ( (0 - decrease-k) * fruitPerTree )
       ;set ntl (fruit-level-of patch-at 0 0) - tl
       ;if ntl < 0 [ set ntl 0 ]
       ;set fruit-level-of patch-at 0 0 ntl
       
       
       setxy go-x go-y  ;put the turlte at the fruit level
       set ntl fruit-level-of patch-at 0 0 
       if ntl > 0 [set fruit-level-of patch-at 0 0 ntl - 1]
       set plabel-of patch-at 0 0 fruit-level
         ; put turtle at fruit
       
       set energy total-energy
         ; change color of cross around fruit to reflect fruit level
       ifelse fruitPerTree > 1
       [set cl cmin-atfruit + (fruit-level-of patch-at 0 0) * (cmax-atfruit - cmin-atfruit) / (fruitPerTree - 1) ]
       [set cl cmin-atfruit]
       if fruit-level-of patch-at 0 0 <= 0
       [
       set pcolor black
       set plabel-of patch-at 0 0 ""
       ] 
       draw-cross cl
       set num-agents-reach-fruit num-agents-reach-fruit + 1
       set heading random 360
       change-state s-explore
    ]
    [ change-state s-adapt-fruit set-heading ]
  ]
  [ change-state s-explore set entitySeen 6 ];if fruit  

end



;---------------------------------



;----------------------------------------------------------------------------------------
; Return x coordinate given distance and angle
to-report get-x [d a]
   report xcor + d * sin (heading + a)
end
;----------------------------------------------------------------------------------------
; Return y coordinate given distance and angle
to-report get-y [d a]
   report ycor + d * cos (heading + a) 
end
;----------------------------------------------------------------------------------------

; Return true if universe is a torus.
; If it is non-torus, then return true only if (x,y) lie within boundaries
to-report valid [x y]
  ifelse torus = false
  [
     ifelse x < xleft or x > xright or y < ybottom or y > ytop 
     [ report false ]
     [ report true ]
  ]
  [ report true ]
end
;----------------------------------------------------------------------------------------






;---------------------------------------------------------------
;---------------------------------------------------------------
;---------------------------------------------------------------
;---------------------------------------------------------------
;    INIT WORLD AND VAR
;---------------------------------------------------------------
;------------------------------------
;set-screen-coors :
;  Utiliser simplement pour redefinir le nom 
;  des variables des limites de l'environnement
;------------------------------------
to set-screen-coors
 ;;;;;;;;;;;;;modif pour adaptation pour  2.1 version ;;;;;;;;;;;;
  set xleft   0 + min-pxcor
  set xright  max-pxcor
  set ytop    max-pycor
  set ybottom 0 + min-pycor
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
end



;;------------------------------------
;setup-general-params :
;  Initialise les variables globals 
;  
;------------------------------------
to setup-general-params

  ;--------------------------
  ;colors associated at each object's model
  set cwall            blue
  set cobstacle        yellow
  set cfruit           red
  set cmin-atfruit     black
  set cmax-atfruit     white
  set ccross-fruit     13
  ;------------------------------
  
  ;------------------------------
  ;Each possible state is associated with an int
  set s-explore        0
  set s-adapt-fruit    1
  set s-adapt-rich     2
  set s-adapt-explorer 3
  set s-adapt-obstacle 4
  set s-leave-fruit    5  

  set s-poor           6
  set s-rich           7
  set s-normal         8
  set s-dead           9
 ;------------------------------
 

  ;----------------------------------------
  ;array allowing the association beetwen a state 
  ; (encoded by the integers above) and a color, given
  ;by the int at the state index
  set agent-color  [  56   ; green    => s-explore
                     126   ; magenta  => s-adapt-fruit
                      46   ; yellow    => s-adapt-rich
                      16   ; red      => s-adapt-explorer
                      116  ; violet   => s-adapt-obstacle
                      96   ; blue sky => s-leave-fruit !!! pls utiliser
                   ]
  ;----------------------------------------             

  
  ;set decrease-k (ln (100 * num-agents)) / 100;;;;;K=? ;??NO MORE USEFUL
  set max-h           90;;;;;;;h=? l'angle droit?
  set num-cooperation 0    ;compteur 
  set num-defection 0      ;compteur
  set dec 70
  set distA 3
  set title "ACACIA Cooperation version"
end


;Some errors because turtles are called in a strange way to build up wall, obstacles and fruits
;So it remains a bug when no torus : impossible to have 0 fruit
to setup-world
  set-default-shape turtles "butterfly"
 
   crt num-obstacles
  ask turtles [setup-obstacles]
  let nf numTrees
  if torus = false
  [
    crt 1
    ask turtles [setup-walls]
    set nf numTrees 
  ]
 
  crt nf
  ask turtles [setup-fruit]
  crt num-agents
  ask turtles [setup-agent  change-shape]
 
  if global-output = true [setup-graph]
  
end


;----------------------------
to setup-agent
  
  setxy random world-width random world-height
  rt random-float 360
  if pcolor = cwall or pcolor = cobstacle or pcolor = cfruit [setup-agent]
  change-state s-explore ;simon:mettre tout les agents "au vert" 
  ifelse same-h = true [ set heading init-h ] [ set heading random 360 ]  
  set last-turn -1
  set last-counter 0 - 1 
  set energy int (total-energy / 2)
  set neighbour -1
  set mycode 600
  set s-see-rich false
  set s-see-fruit false
end


;----------------------------
;setup-memory (turtle point of view):
;   depending on choice made graphically, set the starting comportement
;   of turtles
;----------------------------
to setup-memory
  locals [i]  
   
   ;;ESS test
;   ifelse curRep < rep
;   [
;     set curRep curRep + 1
;     set strategy 0 change-shape
;   ]
;   [set strategy 2 change-shape]
   
   ;----------------------------
   ;;fixed pop test 
  if Distribution-Agents = "selfish" [set strategy 0 change-shape]
  if Distribution-Agents = "realist" [set strategy 1 change-shape]
  if Distribution-Agents = "kind" [set strategy 2 change-shape]
;  if Distribution-Agents = "1/3kind1/3realist1/3selfish" [set strategy random 3 change-shape]
;;;-------------------
  set experience 0
  set last-seen -1
  set selfish-flag 0
  ifelse  TFT-Start = "B" 
  [ 
   ifelse (random 2) = 0 [ 
      ifelse(big-memory)[ set last-coop n-values num-agents ["D"]] [set last-coop "D"] 
    ]
    [ 
      ifelse(big-memory)[ set last-coop n-values num-agents ["C"]] [set last-coop "C"] 
    ]
  ]
  [ 
   ifelse(big-memory)[ set last-coop n-values num-agents [TFT-Start] ] [set last-coop TFT-Start] 
  ]
end
;----------------------------------------------------------------------------------------

; Draw walls

to setup-walls
   
   set pcolor cwall
   ask patch 0 0 [set pcolor black]
   ask patches with [abs pxcor = max-pxcor] [set pcolor cwall]
   ask patches with [abs pycor = max-pycor] [set pcolor cwall]    
   die
end

;-----------------------------------
; Draw fruits at random places
to setup-fruit
    ; stamp fruit and set level
     put-fruit
     set pcolor cfruit 
     set fruit-level-of patch-at 0 0 fruitPerTree  ; assign maximum level to fruit
     set plabel-of patch-at 0 0 fruit-level
     draw-cross ccross-fruit  
     die
end
;----------------------------------------------------------------------------------------
;
; Try free positions to put a fruit 
to put-fruit
  setxy random world-width random world-height
  if pcolor = cwall or pcolor = cobstacle or pcolor = cfruit [put-fruit]
  if pcolor-of (patch-at-heading-and-distance 0 1) = cwall or pcolor-of (patch-at-heading-and-distance 0 1) = cobstacle [put-fruit]
  if pcolor-of (patch-at-heading-and-distance 90 1) = cwall or pcolor-of (patch-at-heading-and-distance 90 1) = cobstacle [put-fruit]
  if pcolor-of (patch-at-heading-and-distance 180 1) = cwall or pcolor-of (patch-at-heading-and-distance 180 1) = cobstacle [put-fruit]
  if pcolor-of (patch-at-heading-and-distance -90 1) = cwall or pcolor-of (patch-at-heading-and-distance -90 1) = cobstacle [put-fruit]
  if pcolor-of (patch-at-heading-and-distance 0 1) = cfruit [put-fruit]
  if pcolor-of (patch-at-heading-and-distance 90 1) = cfruit [put-fruit]
  if pcolor-of (patch-at-heading-and-distance 180 1) = cfruit [put-fruit]
  if pcolor-of (patch-at-heading-and-distance -90 1) = cfruit [put-fruit]
end
;----------------------------------------------------------------------------------------
;----------------------------------

;------------------------
; Draw cross around fruit
to draw-cross [cross-color]
     
     let n 0
     let w 0
     repeat 4[ 
       set heading n
       set w pcolor-of (patch-ahead 1)                        
       if (w != cwall and w != cobstacle and w != cfruit)
       [ Set pcolor-of (patch-ahead 1) cross-color ]        
       set n n + 90
     ] 
end
;-------------------------


;------------------------------------------------------------------------------
; Turtle procedures
; Draw universe: Make turtles draw obstacles
to setup-obstacles
  locals [angle ww hh] ;simon : on va creer les obstacles 
  set angle 0 
  set ww 0 
  set hh 0
  set color cobstacle
  ifelse regular-obstacles = true 
  [
       randomize
        set heading 180
        set ww random obstacle-mean-width
        set hh random obstacle-mean-height
        repeat ww 
        [ 
           
           repeat hh [ fd 1 set pcolor-of (patch-at 1 1) cobstacle ]
           set heading 0
           
           repeat hh [ fd 1 set pcolor-of (patch-at 1 1) cobstacle ]
           set heading 90
           fd 1
           set heading 180                      
        ]
  ]  
  [     
       if obstacle-solidity > 0 [ set angle-step 360 / obstacle-solidity ] ;condition inverse inateniable
       randomize
       let rdm 0
       set rdm random obstacle-thickness ;choose a thickness 
       repeat rdm
       [ 
    
          if obstacle-solidity > 0
          [
            set angle 0
            repeat obstacle-solidity
            [
             set pcolor-of (patch-at-heading-and-distance angle 1) cobstacle
             set angle angle + angle-step
            ]
          ]
          fd 1 lt random 360 rt random 360
        ]
  ]
  die
end
;----------------------------------------------------------------------------------------

; Draw agent at its initial position
;Modification pour 3.0 
to randomize
  setxy random-xcor random-ycor
  if pcolor = cwall      ; if it's on the wall...
    [ randomize ]        ; ...try again
end
;----------------------------------------------------------------------------------------
;---------------------------------------------------------------
;---------------------------------------------------------------
;---------------------------------------------------------------
;---------------------------------------------------------------


;------------------------------------------------------------------------
;------------------------------------------------------------------------
;------------------------------------------------------------------------
;        FUNCTION USED TO PRINT PLOTS AND THE OUTPUT FILE
;
; I (Simon C.) have choose to not keep the old "acacia plot" up to date.
; they remain but are almost empty, however all informations needed to made 
; more beautiful plot is now available in an output FILE inside the 
; "../data" directory
;------------------------------------------------------------------------
;------------------------------------------------------------------------
; to print the header

to show-global-header 
  locals [line free-space yesno]
  clear-output
  set line "--------------------------------------------------------"
  set free-space 0  
  set yesno ""

  ;if print-header = true
  ;[
    ifelse torus = false
    [
       set yesno "No"
       ;MODIFICATION pour 2.0
       set free-space trunc (100 * (1 - (count patches with [pcolor = cobstacle]) / (world-width * world-height )))
    ] 
    [
       set yesno "Yes"
       ;MODIFICATION pour 2.0
      set free-space trunc (100 * (1 - (count patches with [pcolor = cobstacle]) / ((world-width - 2) * (world-height - 2)))) 
    ]
    file-print title
    file-print line
    ;MODIFICATION
    file-print      "The world settings      : "
    file-print word "Torus world             : " yesno 
    file-print word "Num Agents              : " num-agents 
    file-print word "Num Obstacles           : " num-obstacles 
    file-print word "Percent of free space   : " free-space 
    file-print line
    file-print      "The adaptive & perceptive settings: "
    ifelse agent-coop = true
   [file-print      "agent-coop     : yes" ]
   [file-print      "agent-coop     : no" ]
    
    ifelse obs-a = true
   [file-print      "Adapt to obstacle       : yes" ]
   [file-print      "Adapt to obstacle       : no" ]

    file-print word "Attention distance      : " attention-distance
    file-print word "Attention angle         : " attention-angle
    file-print word "Attention resolution    : " attention-resolution
    file-print word "Head tolerance          : " head-tolerance
    file-print line
    file-print      "The energy settings     : "
    file-print word "Energy maximum of agent : " total-energy
    file-print word "Number of initial fruits: " numTrees 
    file-print word "Fruit sharing           : " fruitPerTree
    file-print word "Fruit's generating speed: " generating-speed 
    file-print line
    file-print      "Agents Behaviour Settings : "
    file-print word "Agents Initial behavior :" Distribution-Agents
    ifelse memory = true
    [file-print     "Memory       : yes"]
    [file-print     "Memory       : no" ] 
    file-print line                
  
  ;]
;full output : high space cost, but old way so maybe still usefull
;  file-print " Nb_Simulation \t Counter  \t  Memory  \t  Ag-Initial-Behav  \t  Obstacle-Adapt  \t  Expl-adapt \t  Rich-adapt \t  Agent-Coop \t  Reg-speed \t  Nb-Obstacle   \t  Fq_alive  \t  Fq_die   \t   Fq_rich   \t   Fq_normal   \t   Fq_poor   \t   Fq_kind   \t   Fq_realist   \t   Fq_selfish    \t   Fq_coop   \t   Fq_defect   \t   interaction \t adaptation-ag \t p-adaptation-Obst \t vital-space \t attention-angle \t attention-resolution \t attention-distance \t Num_Fruits \t Rep" ;///////////////////////////////////////////////////////////////
;
  file-print "sim_num \t time \t rs \t no \t n_alive \t n_kind \t n_realist \t n_selfish \t vital-space \t nf \t Rep" ;///////////////////////////////////////////////////////////////

end
;------------------------------------------------------------------------

;------------------------------------------------------------------------
;------------------------------------------------------------------------
;  to plot 

to show-global-results
locals [p-alive p-rich p-normal p-poor p-die p-good p-realist p-bad p-coop p-defect p-interaction p-adaptation-Ag-Ag p-adaptation-Ag-Obst p-adaptation-Obst
        n-alive n-rich n-normal n-poor n-die n-good n-realist n-bad ]
set n-rich count turtles with [e-state = s-rich] 
set n-normal count turtles with [e-state = s-normal]
set n-poor  count turtles with [e-state = s-poor and hidden? = false]
set n-die count turtles with [hidden? = true]
set n-good count turtles with [strategy = 2 and hidden? = false]
set n-realist count turtles with [strategy = 1 and hidden? = false]
set n-bad count turtles with [strategy = 0 and hidden? = false]
set n-alive n-good + n-bad + n-realist

set p-alive (n-alive)* 100 / num-agents
set p-die  n-die * 100 / num-agents
ifelse n-alive > 0
[
set p-rich n-rich * 100 / n-alive
set p-normal n-normal * 100 / n-alive
set p-poor n-poor * 100 / n-alive
;set p-poor 100 - p-rich - p-normal
if p-poor < 0 [set p-poor 0]
set p-good n-good 
set p-realist n-realist
set p-bad n-bad
;set p-bad 100 - p-good - p-realist
if p-bad < 0 [set p-bad 0]
]
[set p-rich 0
 set p-normal 0
 set p-poor 0
 set p-good 0
 set p-realist 0
 set p-bad 0
]
ifelse num-adaptation-agent-rich + num-adaptation-agent-explorer = 0
[set p-richa-a 0
set p-expla-a 0]
[set p-richa-a num-adaptation-agent-rich * 100 / (num-adaptation-agent-rich + num-adaptation-agent-explorer)
set p-expla-a num-adaptation-agent-explorer * 100 / (num-adaptation-agent-explorer + num-adaptation-agent-rich )
]
ifelse num-cooperation + num-defection = 0
[set p-coop 0
 set p-defect 0]
[set p-coop num-cooperation * 100 / (num-cooperation + num-defection)
 set p-defect num-defection * 100 / (num-cooperation + num-defection)
]
;  set-current-plot "energy-plot"
;  set-current-plot-pen "rich"
;  plot p-rich
;  set-current-plot-pen "normal"
;  plot p-normal
;  set-current-plot-pen "poor"
;  plot p-poor
;  set-current-plot "strategy-plot"
;  set-current-plot-pen "good"
;  plot p-good
;  set-current-plot-pen "realist"
;  plot p-realist
;  set-current-plot-pen "bad"
;  plot p-bad
;  set-current-plot "alive-plot"
;  set-current-plot-pen "alive"
;  plot p-alive
;  set-current-plot-pen "dead"
;  plot p-die
  set-current-plot "cooperation-plot"
  set-current-plot-pen "coop"
  plot p-coop
;  set-current-plot-pen "defect"
;  plot p-defect
;  set-current-plot "Agent-Adapt-plot"
;  set-current-plot-pen "rich"
;  plot p-richa-a
;  set-current-plot-pen "explorer"
;  plot p-expla-a

  set p-interaction num-cooperation + num-defection + num-adaptation-agent
  set p-adaptation-Ag-Ag num-adaptation-agent
  set p-adaptation-Ag-Obst num-adaptation-obstacle
if print-every > 0 and (count-steps mod print-every) = 0
[
file-print "S"+ count-sims + "\t" + count-steps +"\t"+ generating-speed + "\t" + num-obstacles +"\t"+ n-alive +"\t"+ n-good +"\t"+ n-realist +"\t"+ n-bad +"\t"+ v-space +"\t"+ fruitPerTree + "\t" + curRep
]

end



to show-case-header
  locals [i w]
  set i 0

  set w "Turtle"
  repeat num-agents
  [
     set w word w " "
     set w word w i 
     set i i + 1
  ]
  file-print w
end


to setup-graph 
;  set-current-plot "energy-plot"
;  set-plot-y-range 0 100
;  set-current-plot "strategy-plot"
;  set-plot-y-range 0 100
;  set-current-plot "alive-plot"
;  set-plot-y-range 0 100
;  set-current-plot "cooperation-plot"
;  set-plot-y-range 0 100
;  set-current-plot "Agent-Adapt-plot"
;  set-plot-y-range  0 100
end

to setup-print
  ifelse global-output = true
  [ show-global-header ]   
  [ show-case-header ] 
end

;---------------------------------------
;---------------------------------------
;print each state of each agent
to show-case-results
  locals [i w]
  set i 0
  set w ""
  set w word w "ST "
  set w word w count-steps
  if print-every > 0 and (count-steps mod print-every) = 0[  
        if count-steps < 100 [set w word w " "]
        if count-steps < 10 [set w word w " "]
        repeat num-agents[ 
            set w word w " "              
            ifelse hidden?-of turtle i = true [set w word w "D"] 
            [
             if e-state-of turtle i = s-rich [set w word w "R"]
             if e-state-of turtle i = s-normal [set w word w "N"]
             if e-state-of turtle i = s-poor [set w word w "P"]
            ]
            set i i + 1
       ]
   file-print w  
  ]
end



to-report trunc [x]
  report (round (x * 100)) / 100
end



;----------------------------------------------------------------------------------------


;------------------------------------------------------------------------
;------------------------------------------------------------------------
;-----------------------------------------------------------------------
;------------------------------------------------------------------------
;------------------------------------------------------------------------
;------------------------------------------------------------------------
;------------------------------------------------------------------------
;------------------------------------------------------------------------
;-----------------------------------------------------------------------
;------------------------------------------------------------------------
;------------------------------------------------------------------------
;------------------------------------------------------------------------
;
@#$#@#$#@
GRAPHICS-WINDOW
331
82
800
572
25
25
9.0
1
7
1
1
1
0
1
1
1
-25
25
-25
25

CC-WINDOW
5
715
1100
810
Command Center
0

SLIDER
260
10
432
43
num-sims
num-sims
1
2000
200
1
1
NIL

SLIDER
496
10
675
43
num-steps
num-steps
0
5000
2000
1
1
runs

SWITCH
808
577
949
610
global-output
global-output
0
1
-1000

BUTTON
9
10
75
43
start
move
NIL
1
T
OBSERVER
NIL
NIL

SWITCH
81
10
171
43
play
play
1
1
-1000

SLIDER
24
495
200
528
attention-resolution
attention-resolution
1
15
9
1
1
point

SLIDER
829
332
937
365
generating-speed
generating-speed
1
7
3
1
1
NIL

SLIDER
36
111
173
144
num-agents
num-agents
1
200
100
1
1
NIL

SLIDER
177
111
310
144
total-energy
total-energy
0
250
200
1
1
NIL

SWITCH
182
326
308
359
agent-coop
agent-coop
0
1
-1000

SWITCH
209
275
304
308
obs-a
obs-a
0
1
-1000

SLIDER
26
532
211
565
attention-angle
attention-angle
1
180
120
1
1
degres

SLIDER
24
457
200
490
attention-distance
attention-distance
0
20
9
1
1
NIL

SWITCH
208
238
303
271
richa-a
richa-a
0
1
-1000

SLIDER
828
297
936
330
fruitPerTree
fruitPerTree
0
108
5
1
1
NIL

SWITCH
16
229
128
262
memory
memory
0
1
-1000

SWITCH
209
203
304
236
expla-a
expla-a
0
1
-1000

SLIDER
25
569
208
602
head-tolerance
head-tolerance
0
180
45
1
1
degrees

SWITCH
971
332
1061
365
torus
torus
1
1
-1000

SLIDER
969
394
1078
427
num-obstacles
num-obstacles
0
1000
0
1
1
NIL

SLIDER
829
259
936
292
numTrees
numTrees
0
20
4
1
1
NIL

SWITCH
26
411
139
444
same-h
same-h
1
1
-1000

SLIDER
27
377
157
410
init-h
init-h
0
360
137
1
1
degres

CHOOSER
844
10
974
55
Distribution-Agents
Distribution-Agents
"selfish" "realist" "kind" "1/3kind1/3realist1/3selfish"
0

SWITCH
970
438
1077
471
regular-obstacles
regular-obstacles
1
1
-1000

SLIDER
815
431
962
464
obstacle-mean-width
obstacle-mean-width
0
20
14
1
1
NIL

SLIDER
816
525
959
558
obstacle-solidity
obstacle-solidity
0
100
7
1
1
NIL

SLIDER
815
487
959
520
obstacle-thickness
obstacle-thickness
0
100
5
1
1
NIL

SLIDER
957
576
1066
609
print-every
print-every
0
2000
1000
25
1
NIL

MONITOR
436
10
486
59
Sim n°
count-sims\n
0
1

MONITOR
679
10
739
59
Step n°
count-steps
0
1

MONITOR
880
128
930
177
Rich :
count turtles with [e-state = s-rich]
0
1

MONITOR
934
129
984
178
Normal
count turtles with [e-state = s-normal]
0
1

MONITOR
988
128
1038
177
Poor
count turtles with [e-state = s-poor]
3
1

MONITOR
1041
128
1091
177
Dead
num-agents - count turtles
0
1

MONITOR
878
65
928
114
Kind
count turtles with [strategy = 2 and hidden? = false]
0
1

MONITOR
988
64
1038
113
Selfish
count turtles with [strategy = 0 and hidden? = false]
0
1

MONITOR
933
63
983
112
Real
count turtles with [strategy = 1 and hidden? = false]
0
1

TEXTBOX
813
80
874
98
Strategy :

TEXTBOX
816
151
863
169
Energy state:

PLOT
332
581
801
701
cooperation-plot
Time
%
0.0
2000.0
0.0
100.0
true
false
PENS
"coop" 1.0 0 -2674135 true
"defect" 1.0 0 -16777216 true

SWITCH
964
193
1085
226
adaptCol
adaptCol
1
1
-1000

MONITOR
818
624
899
673
vital space
v-space
3
1

MONITOR
970
626
1027
675
to
(( count patches - count patches with [pcolor = cobstacle or pcolor = cwall] )   / count patches ) * generating-speed * fruitPerTree
3
1

SLIDER
815
393
962
426
obstacle-mean-height
obstacle-mean-height
0
100
80
1
1
NIL

TEXTBOX
808
375
950
393
reg shape :

TEXTBOX
804
466
954
484
unreg shape :\n

CHOOSER
987
10
1079
55
TFT-Start
TFT-Start
"D" "C" "B"
1

SWITCH
16
267
155
300
big-memory
big-memory
1
1
-1000

TEXTBOX
808
238
958
256
Fruit conf.:

TEXTBOX
808
203
958
221
Env. Settings:

TEXTBOX
6
67
156
85
Agents settings :

TEXTBOX
18
352
168
370
Visual abilities:

TEXTBOX
14
200
164
218
Memory abilities

TEXTBOX
171
174
321
192
Adapt behaviors:

TEXTBOX
14
89
164
107
Gnl:

@#$#@#$#@
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

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

butterfly_kind
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -13840069 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

butterfly_kind1
true
0
Polygon -13840069 true false 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -13840069 true false 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -13840069 true false 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -13840069 true false 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60
Polygon -7500403 true true 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225

butterfly_real
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -1 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

butterfly_real1
true
0
Polygon -1 true false 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -1 true false 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -1 true false 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -1 true false 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -7500403 true true 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

butterfly_self
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -2674135 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

butterfly_self1
true
0
Polygon -2674135 true false 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -2674135 true false 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -2674135 true false 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -2674135 true false 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -7500403 true true 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
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
NetLogo 3.1.5
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
