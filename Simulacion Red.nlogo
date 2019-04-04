globals [
  available-colors      ;; list of colors that will be assigned to points
  current-point         ;; the point the user is currently moving
  numpoints   ;;
  xlist
  ylist
  vallist
  onlist
]

breed [points point]    ;; these are the little circles in the middle of the polymodificaPuntosns
points-own [value super?]
undirected-link-breed [edges edge]
breed [triangles triangle]
triangles-own [Ux Uy rad2 bad? vertices sides]
edges-own [keep?]


;; The next two breeds are used only when we're updating the polymodificaPuntosns
;; when the user moves the points with the mouse.  See below for further
;; details.
breed [spawners spawner]
breed [updaters updater]

;;;
;;; CORE PROCEDURES
;;; These are the only procedures necessary to draw the diagram
;;; initially, without moving points.
;;;

to setup
  clear-all
  ;; too dark and too light are hard to distinguish from each other,
  ;; so only use 13-17, 23-27, ..., 133-137
  set available-colors shuffle filter [ c ->
    (c mod 10 >= 3) and (c mod 10 <= 7)
  ] n-values 140 [ n -> n ]
  set-default-shape points "circle 3"
  ask n-of number patches [ make-point ]
  ask patches [ recolor ]
  set current-point nobody
  reset-ticks
end

to make-point ; patch procedure
  sprout-points 1 [
    set size 9
    set value random 100 ; Nos da la importancia de cada nodo
    ifelse value > 80 [set color red]
    [ifelse value > 60 [set color orange][
      ifelse value > 40[set color yellow][
        ifelse value > 20 [set color green][set color blue]]]]
    set available-colors butfirst available-colors
  ]
end

to recolor  ;; can be patch or turtle procedure
  set pcolor [color] of min-one-of points [distance myself]
end


;;;
;;; OTHER PROCEDURES
;;; The rest of the procedures are used for efficiently updating
;;; the diagram when the user moves the points around.
;;;

to modificaPuntos
  obey-mouse
  ask spawners [ spawn ]
  ask updaters [ update ]
end

to modificaImportancia
  obey-mouse2
  ask spawners [ spawn ]
  ask updaters [ update ]
end

;; Codigo que genera una red de transporte usando la triangulacion de Delaunay
to generaRed

  set numpoints number
  set onlist (list points)
  set xlist [xcor] of points
  set ylist [ycor] of points
  set vallist [value] of points

  set onlist fput false onlist
  set xlist fput 0 xlist
  set ylist fput 0 ylist
  set vallist fput 0 vallist
  clear-drawing
  ;ask points[die]

  if max xlist > max-pxcor - 1 or max ylist > max-pycor - 1 or min xlist < min-pxcor + 1 or min ylist < min-pycor + 1
     [user-message "One or more coordinate values are outside the screen range.  Please check your inputs and setup again"
      stop]
  create-points numpoints + 1 [ if who != 0 [set shape "circle" set size .6 set color red]
                      setxy item who xlist item who ylist
                      set value item who vallist
                      set label who set label-color black
                      set hidden? not item who onlist
                      set super? false
                      face patch 0 0
  ]
  ask points with [not hidden?] [if distance min-one-of other points with [not hidden?] [distance myself] = 0
    [user-message "Two or more active points have identical coordinates.  Please halt, revise your inputs and setup again" stop]]
  create-points 1 [set hidden? true set super? true setxy (min-pxcor + 1) (min-pycor + 1)] ; who numpoints + 1
  create-points 1 [set hidden? true set super? true setxy (min-pxcor + 1) (max-pycor - 1)] ; who numpoints + 2
  create-points 1 [set hidden? true set super? true setxy (max-pxcor - 1) (max-pycor - 1)] ; who numpoints + 3
  create-points 1 [set hidden? true set super? true setxy (max-pxcor - 1) (min-pycor + 1)] ; who numpoints + 4
end

to triangulate
  setup
  ifelse count points with [not hidden?] < 3 [user-message "You will need at least three active points to triangulate"]
  [

  make-triangle point (numpoints + 1) point (numpoints + 2) point (numpoints + 3) ; super-triangle 1
  make-triangle point (numpoints + 3) point (numpoints + 4) point (numpoints + 1) ; super-triangle 2
  let triset (sort-on [xcor] points with [not hidden?])
  foreach triset ; for each point in the pointlist
    [p ->
     ask triangles [if inside-circum? p self [set bad? true]] ; for each triangle in the triangulation flag points within circumcircle

     let edgelist []
     ask triangles with [bad?] ; for each new bad triangle
      [
        if Pause-between-increments? [ask link-set sides [set color black] ask p [set size 2] user-message "Next step?" ask p [set size 1]]
        ask link-set sides [set edgelist ifelse-value member? self edgelist [remove self edgelist][lput self edgelist]]]
     clean-up
     foreach edgelist [ed -> make-triangle [end1] of ed [end2] of ed p];create new triangles

     ]
    ask triangles with [((member? point (numpoints + 1) vertices) or (member? point (numpoints + 2) vertices)
                      or (member? point (numpoints + 3) vertices) or (member? point (numpoints + 4) vertices))][set bad? true]
    if Pause-between-increments? [ user-message "Complete triangulation?"]
    clean-up
    ask links with [color = red][die]
   ]
end

to make-triangle [p1 p2 p3]
  let me 0
  create-triangles 1
   [set hidden? true
    set bad? false
    set vertices turtle-set (list p1 p2 p3)
    let Ax [xcor] of p1
    let Ay [ycor] of p1
    let Bx [xcor] of p2
    let By [ycor] of p2
    let Cx [xcor] of p3
    let Cy [ycor] of p3
    let midx (max-pxcor + min-pxcor) / 2
    let midy (max-pycor + min-pycor) / 2
    if [super?] of p1 [set Ax midx - 10000 * (midx - Ax) set Ay midy - 10000 * (midy - Ay)] ; super vertices to "infinity"
    if [super?] of p2 [set Bx midx - 10000 * (midx - Bx) set By midy - 10000 * (midy - By)]
    if [super?] of p3 [set Cx midx - 10000 * (midx - Cx) set Cy midy - 10000 * (midy - Cy)]
    let d1x Bx - Ax
    let d1y Ay - By
    let d2x Cx - Bx
    let d2y By - Cy
    let d3x Ax - Cx
    let d3y Cy - Ay
    let A2 (Ax ^ 2 + Ay ^ 2)
    let B2 (Bx ^ 2 + By ^ 2)
    let C2 (Cx ^ 2 + Cy ^ 2)
    let D 2 * (Ax * d2y + Bx * d3y + Cx * d1y)
    set Ux (A2 * d2y + B2 * d3y + C2 * d1y) / D
    set Uy (A2 * d2x + B2 * d3x + C2 * d1x) / D
    set rad2 ((Ax - Ux) ^ 2 + (Ay - Uy) ^ 2)
    set me who
   ]
   ask [vertices] of triangle me [create-edges-with other [vertices] of triangle me [set hidden? false]]
   let vwho [who] of [vertices] of triangle me
   ask triangle me [set sides (list edge item 0 vwho item 1 vwho edge item 1 vwho item 2 vwho edge item 2 vwho item 0 vwho)]
end

to-report inside-circum? [pt tri]
 let r1sq ([xcor] of pt - [Ux] of tri ) ^ 2 + ([ycor] of pt - [Uy] of tri ) ^ 2
 report r1sq < [rad2] of tri
end

to clean-up
  ask edges [set keep? false]
  ask triangles [ifelse bad? [die][ask link-set sides [set keep? true set color green]]];remove bad triangles
  ask edges with [not keep?][set color red] ;remove unused edges
end

;;
;;


to obey-mouse
  ;; first handle the case where the user has released the mouse button
  ;; (or hasn't pressed it yet)
  if not mouse-down? [
    set current-point nobody
    stop
  ]
  ;; if the mouse button is down, get the mouse position
  let x round mouse-xcor
  let y round mouse-ycor
  ;; if we don't have a point yet, pick the closest one
  if current-point = nobody [
    set current-point min-one-of points [distancexy x y]
  ]
  ;; check if the point needs to move
  if x != [xcor] of current-point or y != [ycor] of current-point [
    ;; move the point
    ask current-point [ setxy x y ]
    ;; the point has moved, so we need to recolor all patches, so we kill off
    ;; the old turtles that were doing the recoloring and make new ones
    ask spawners [ die ]
    ask updaters [ die ]
    ask current-point [ ask patch-here [ make-spawners ] ]
  ]
end

to obey-mouse2
  ;; first handle the case where the user has released the mouse button
  ;; (or hasn't pressed it yet)
  if not mouse-down? [
    set current-point nobody
    stop
  ]
  ;; if the mouse button is down, get the mouse position
  let x round mouse-xcor
  let y round mouse-ycor
  ;; if we don't have a point yet, pick the closest one
  if current-point = nobody [
    set current-point min-one-of points [distancexy x y]
  ]
    ;; We now change the color of the patch
    ask current-point [
    set value Importancia
        ifelse value > 80 [set color red]
    [ifelse value > 60 [set color orange][
      ifelse value > 40[set color yellow][
        ifelse value > 20 [set color green][set color blue]]]]
  ]

    ;; the point has moved, so we need to recolor all patches, so we kill off
    ;; the old turtles that were doing the recoloring and make new ones
    ask spawners [ die ]
    ask updaters [ die ]
    ask current-point [ ask patch-here [ make-spawners ] ]
end

;; Here's how we use turtles to update the patches in a growing
;; square pattern.  We use two breeds of turtles, spawners and updaters.
;; Spawners are at the corners of the square and move diamodificaPuntosnally.  Each
;; time a spawner moves, it spawns two new updaters.  The updaters move
;; vertically or horizontally, and every time an updater lands on a patch,
;; it recolors it.  When a spawner or an updater hits the edge of the world,
;; it dies.  Together, these rules are enough to make the growing square!

to make-spawners  ;; patch procedure
  let counter 0
  sprout-spawners 4 [
    ;; give the four headings of NE, SE, SW, and NE
    set heading 45 + counter * 90
    set counter counter + 1
    set color gray
    if not show-updates? [ hide-turtle ]
  ]
end

to spawn  ;; spawner procedure
  hatch-updaters 1 [ rt 45 ]
  hatch-updaters 1 [ lt 45 ]
  if not can-move? 1 [ die ]
  ;; Moving diamodificaPuntosnally is a little tricky.  Moving forward 1 isn't enough by
  ;; itself, since that doesn't take us all the way to the center of the
  ;; diamodificaPuntosnally next patch.  So after moving forward, we use SETXY to move
  ;; to the exact center of the new patch.
  fd 1
  setxy pxcor pycor
end

to update  ;; updater procedure
  recolor
  if not can-move? 1 [ die ]
  fd 1
end


; Copyright 2006 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
200
10
809
620
-1
-1
1.0
1
10
1
1
1
0
0
0
1
-300
300
-300
300
0
0
0
ticks
30.0

BUTTON
22
74
164
107
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
1

SLIDER
8
36
185
69
number
number
1.0
70.0
70.0
1.0
1
NIL
HORIZONTAL

BUTTON
11
165
183
258
NIL
modificaPuntos\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SWITCH
20
111
171
144
show-updates?
show-updates?
0
1
-1000

BUTTON
13
319
155
352
NIL
modificaImportancia
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
26
377
134
410
NIL
generaRed
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
11
276
183
309
Importancia
Importancia
0
100
87.0
1
1
NIL
HORIZONTAL

SWITCH
866
30
1078
63
Pause-between-increments?
Pause-between-increments?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

This model draws a Voronoi diagram of polygons around a set of points.  These diagrams resemble many phenomena in the world including cells, forest canopies, territories of animals, fur and shell patterns, crystal growth and grain growth, cracks in dried mud and other geological phenomena, road networks, and so on.  Voronoi diagrams are useful in computer graphics, vision and path planning for robots, marketing, and other applications.

## HOW IT WORKS

First the points are placed randomly.  Then the polygons are drawn according to the following rules.  Each point is enclosed inside exactly one polygon.  All of the points inside the polygon are closer to that point than they are to any of the other points.

Instead of calculating the mathematically exact coordinates of the polygons, this model constructs an approximation using a grid.  Each grid cell (each "patch", in NetLogo terminology) is colored according to which point it is closest to.

## HOW TO USE IT

Use the NUMBER slider to choose how many points you want, then press SETUP.  The model will place the points and draw the polygons.

If you want to play with moving the points around yourself, press the GO button.  Now you can drag the points around with the mouse.  As you move a point, the model redraws the polygon.  This takes time, so it redraws them starting near the mouse and proceeding outward.  If you want to see the boundary of the updated region, turn on the SHOW-UPDATES? switch.

## THINGS TO NOTICE

The line segment separating two points is exactly midway between them.

How many sides do the polygons typically have?  (You may want to ignore the polygons around the edges.)

Where different colors touch, there is usually a "Y".  When do you get a "T" or an "X" instead?

## THINGS TO TRY

Experiment with the effect of moving the points around.  Moving the points slowly is best.  (If you move them too fast, the model will have trouble keeping up and it won't be easy to see what's going on.)

Align two points so they have the exact same x coordinate or y coordinate.  Is the line between them always perfectly smooth?  (To see the effect, you may have to move the points closer or farther away from each other.  Look closely.)  Also try putting two points exactly on top of each other.  What happens?  Both effects occur because when a grid square ("patch") is equally distant from two differently colored points, NetLogo resolves the tie randomly.

## EXTENDING THE MODEL

Instead of placing the points completely randomly, have them move away from each other until they are roughly equidistant from each other.  This makes all the polygons roughly the same size.

Edit the view and turn wrapping on in both directions, and click SETUP.  The model may seem to be working, but there is a problem.  If you turn on SHOW-UPDATES?, you can see that the update rectangle keeps going forever, continually refreshing the grid colors.  Fix the model to work with wrapping, so that update stops as soon as the whole screen has been redrawn.

Instead of using the patches to display Voronoi polygons, find the boundaries by using turtles.  Create a large batch of turtles at each point (colored the same color as the point), each turtle facing a different angle.  Have the turtles walk outward from their points at a uniform rate.  Stop the turtles when they run into a turtle of a different color.

Instead of using a patch-based approximation, calculate the exact positions of the sides of the polygons.  (There are numerous published algorithms for calculating this information.)  Then display the polygons using turtles with the "line" shape.

## NETLOGO FEATURES

The core procedure for drawing the polygons is called `recolor`. It is only one line long! It puts the `min-one-of` and `distance` reporters to good use.

The `mouse-down?`, `mouse-xcor`, and `mouse-ycor` primitives are used so the user can interact with the model.

Because the number of patches is so large, it takes a while to update them all when a point moves.  So we use moving turtles to recolor the patches; the moving turtles start where the mouse is, and move outwards in a square, since near the mouse is where the user will be looking first.  See the Code tab for the details on how it works.

## RELATED MODELS

* MaterialSim Grain Growth
* Fur
* Honeycomb
* Scatter

## CREDITS AND REFERENCES

For more information on Voronoi diagrams, see https://en.wikipedia.org/wiki/Voronoi.  (There are also many other sites on this topic on the web.)

Thanks to John Jungck from Beloit College for inspiring this model with his talk at Northwestern University about Voronoi structures in nature.

Thanks to Josh Unterman and Seth Tisue for their work on this model.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (2006).  NetLogo Voronoi model.  http://ccl.northwestern.edu/netlogo/models/Voronoi.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2006 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2006 -->
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

circle 3
false
0
Circle -16777216 true false 0 0 300
Circle -7500403 true true 30 30 240

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
NetLogo 6.0.4
@#$#@#$#@
setup
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
