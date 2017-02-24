;; Global declarations -------------------------------------------------------------------
extensions [ array ]

;; just declare the globals first, assign in "setup"
globals [
  ticks-per-day  ;; allows intra-day dynamics (de facto definition of "hours", or "periods")
  is-day?        ;; day vs. night boolean
  sim-day        ;; simulation day (ticks / ticks-per-day)
  time-of-day    ;; in hours
  exposed-min    ;; min/max for latency period probability distribution (Rayleigh)
  exposed-max    ;;
  infectious-min ;; min/max for infectious period probability distribution (Rayleigh)
  infectious-max ;; 
  eip-min        ;; min/max for extrinsic incubation period probability distribution (Rayleigh)
  eip-max        ;; 
  sim-timer      ;; simulation timer (calculation time only)
  perf-timer     ;; performance timer
  tempimmune-time ;; duration of the temporary immunity, following 1st infection (in days)

  hwidth         ;; width of a house (in meters)
  xsep           ;; x separation between house centers (in meters)
  ysep           ;; y separation between house centers (in meters)
  setup-run-done?
  
  Initial-People ;; number of people with which to seed the simulation
  Mosquito-Migration-Step ;; size (in patches) of mosquito step
  Person-Migration-Step ;; size (in patches) of person step
  
  ;; disease states (colors)
  SUSCEPTIBLE    ;; never exposed
  EXPOSED        ;; exposed, but not yet infectious
  INFECTIOUS     ;; fully infectious
  TEMPIMMUNE     ;; temporarily immuned -- following 1st infection
  RECOVERED      ;; recovered from 1 strain
  IMMUNED        ;; recovered twice
  ;; vector control
  SPRAYS
  SPRAYE
  SPRAYI

  ;; building types (colors)
  HOUSE          ;; 
  WORK           ;; 
  SCHOOL         ;; 
  OUTDOOR        ;; all outdoor areas (including e.g. farms/gardens)
  BORDER
  ;; vector control
  HOUSE1         ;; 1: allow to vector control, original: not allow to vector control
  SPRAY          ;; sprayed male
  SCENTER        ;; Center of the sprayed intervention
  RCENTER
  LCENTER
  
  buildings      ;; list/matrix to hold building parameters
  workplaces     ;; weighted list of workplace tiles.  Adults go here during the workday.
  schools        ;; weighted list of school tiles.     Kids go here during the day.
  Village-Size   ;; determined from population, houses, buildings, etc...
  Village-Patches ;; only those where houses are, excludes border areas
  bldg-density  ;; number of properties in "village-patches" with houses vs. without houses
  max-bldgs
  continue-biting-prob ;; probability to continue biting
  
  movie-on?

  palette        ;; color palette for displaying fields
  
  all-germs      ;; matrix that holds infection seed characteristics
  finalized?     ;; so as not to write final summary twice

  container-size-cdf           ;; cdf for container sizes
  container-size-sum           ;; integrated, so we can get initial density right
  brood-cdf                    ;; cdf for # of eggs in a brood
  mosquito-mortality-rate-base ;; base hourly mortality rate
  mosquito-mortality-rate-inc  ;; increment (per day, linear increase)
  mosquito-mortality-rate-base-w ;; base hourly mortality rate
  mosquito-mortality-rate-inc-w  ;; increment (per day, linear increase)

  wolbachia-sites
  radial-count-result ;; keep this here...
  all-hidden?
  wolbachia-hidden?
  nhouses
  nproperties ;; within the village limits, without regard for what's on them...
  egg-index
  larva-index
  roads
  travel-profile
  migration-interval
  migration-percent

  bldg-xmin
  bldg-xmax
  bldg-ymin
  bldg-ymax
  tmp-wavc
  tmp-rs
  
  wolb-affect-adult-mortality-SCF
  wolb-affect-fecundity-SCF
  wolb-affect-mating-SCF
  wolb-affect-larva-SCF
  wolb-affect-egg-SCF

  n-contact
  n-mosquito
  n-iv-contact
  ;n-i-mosquito
  n-ih-contact
  ;n-i-human

  ;; vector control
  ;is-vector-control
  ;control-init-day ;; the start day of vector control intervention
  ;is-reduction-control
  ;reduction-init-day
  ;is-larvicide-control
  ;larvicide-init-day
  ;compliance-ratio ;; set compliance condition to house
  ;cover-radius
  ;control-percent-eggs
  ;control-percent-larvae
  ;control-percent-mosquito
  mortality-rate-mosquito-base
  mortality-rate-mosquito-decay
  mortality-rate-larvae-base
  mortality-rate-larvae-decay
  ;;effective-periods-mosquito
  ;symptomatic-ratio
  n-schedule
  n-reduction
  n-larvicide
  ;spray-sprout-number
  ;; seasonality
  ;peak-day   ;; peak of the ovi-site capacity

  ;; bite matrix (susceptible/infectious vector by susceptible/infectious host)
  bite-matrix
  host-incidence
  vector-incidence
]

;; define the various agent types (agentsets)
breed [mosquitos mosquito] ;; mosquitos, the disease vectors
breed [people person]      ;; human hosts
breed [germs germ]         ;; for convenience: used to seed the initial infection state (not an animated agent)
breed [males male]         ;; male mosquitos, truly a whole other breed... ;-)
;; vector control
breed [sprayed onesprayed] ;; places that are sprayed
breed [reductions reduction] ;; places that are reduced containers
breed [larvicidesprayed onelarvicidesprayed] ;; places that are sprayed by larvicide

;; add attributes to all agents
turtles-own [
  bornx       ;; x coordinate where agent was born (started)
  borny       ;; y coordinate ...
              ;; bornx/y are used to track how far the agents travels on aggregate
  itime       ;; time (in ticks) for infection to move to the next state
              ;; 0, if agent is in a stable state
  strain      ;; current infection type
  infectedx   ;; x coordinate where this agent got infected
  infectedy   ;; y coordinate ...
  infector    ;; id of the agent who infected me
  infectedx-1 ;; x coordinate where the previous agent got infected
  infectedy-1 ;; y coordinate ...
  infector-1  ;; id of the agentn who infected my infector
  age         ;; people: in years, Mosquitos: in days
  igen        ;; infection generation
  averbose?   ;; verbose agent
]

;; add attributes to just people...
people-own [
  first-strain    ;; first infection type
  wrkx            ;; workplace / school coordinates, people are here during the day
  wrky
  hhsize          ;; size of household (0 if not head)
  ;; vector control
  symptomatic     ;; symptomatic or not
]

;; add attributes to just mosquitos...
mosquitos-own [
  age-max      ;; predetermined day of death (how cyclopian)
  body-clock   ;; if > 0 then mosquito can bear children again (in days)
  hunger-clock ;; if > 0 then mosquitos are ready for another bite (like a digestion time, in hours)
  hungry?      ;; controls multi-part blood meals
  bites        ;; # of bites so far in this gonotrophic cycle (must have at least 1 blood feeding to bear children)
  wolbachia?   ;; Wolbachia infection true/false
  mated?       ;; only the first attempt matters to the female, although males will copulate as often as possible
  sterile?     ;; can result from cytoplasmic incompatibility
  go-straight  ;; number of steps a female should just charge on to find another ovi-site, after first ovoposit
  eggs-remaining ;; for partial oviposition
  migrant?     ;; wrapped to the other side?
  vector-capable? ;; false, if they've got Wolbachia (with Wolb-Affect-Vectorial-Capacity probability)
  ;; vector control
  spray-covered        ;; has been sprayed
  own-sprayed-start-day ;; intervention start day
]

males-own [
  age-max      ;; predetermined day of death (how cyclopian)
  wolbachia?   ;; Wolbachia infection true/false
  migrant?     ;; wrapped to the other side?
  ;; vector control
  spray-covered         ;; has been sprayed
  own-sprayed-start-day ;; intervention start day
]

;; add attributes to just germs (convenience turtle to seed infectinos)
germs-own [
               ;; they already have xcor/ycor
               ;; they already have strain (1-4, or 0 for "any")
  target       ;; 1 for mosquito, 2 for people
  radius       ;; radius over which they will reach out
  number       ;; number of agents they will infect (in radius)
  percent      ;; percentage of agents they will infect (in radius) --> set one or the other, not both
]

;; vector control
sprayed-own [
  spray-day    ;; day to spray
]

reductions-own [
  reduction-day ;; day to reduce container
]

larvicidesprayed-own [
  larvicidespray-day  ;; day to spray larvicide
]

patches-own [
  bldg         ;; building type (a color)
  ovi-site?    ;; does this patch have an oviposition site?
  ovi-dens     ;; oviposition site density (larvae / L)
  ovi-site-k     ;; carrying-capacity of ovi-sites
  ovi-site-k-max ;; max-carrying-capacity of ovi-sites
  ovi-scf      ;; for density-dependent ovisite attraction
  mu-max       ;; maximum larval daily mortality
  host-dens    ;; host (human density, time-lagged?)
  mosq-dens    ;; mosquito density
  wovi-dens    ;; (anti-)weighted oviposition density field (subtract mosquito density)
  wolb-dens    ;; wolbachia density (in various life-stages)
  pverbose?    ;; patch verbose
  eggs         ;; list of eggs, ordered by date (hour)
  n-eggs       ;; count...
  larvae       ;; list of larvae, ordered by date (hour)
  n-larvae     ;; count...
  eggs-w       ;; list of eggs, ordered by date (hour)
  n-eggs-w     ;; count...
  larvae-w     ;; list of larvae, ordered by date (hour)
  n-larvae-w   ;; count...
  egg-ages
  larva-ages
  is-road?
  front-door-id
  shade        ;; proxy for desirability of a location to mosquitos who desire rest
  ;; vector control
  larvicide-sprayed ;; has been sprayed
  own-larvicidesprayed-start-day ;; intervention start day
]

;; per-run initial setup --------------------------------------------------------
to Setup
  set movie-on? false

  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  file-close-all
  increment-run-number
  
;; put this back after running BehaviorSpace...  
;; movie-cancel
  reset-timer
  
  if Random-Seed-Value != 0 [random-seed Random-Seed-Value]

  ;; assign globals
  set ticks-per-day 24
  set is-day? true
  set exposed-min 4.5
  set exposed-max 7.5
  set infectious-min 4
  set infectious-max 6
  set eip-min 10
  set eip-max 14  
  set sim-timer 0
  set perf-timer 0
  set tempimmune-time 90 ;; 3 months
  set brood-cdf [[1 .8] [2 1.0]]
;  set container-size-cdf [[10 .2] [50 .5] [100 .9] [1000 1]] ;; in Liters, rough sample based on conversations with Tita
  set container-size-cdf (list (list (Inflate-K * K-Per-House * 100 / Breteau-Index) 1)) ;; all one size
  let i 0
  let lastval 0
  set container-size-sum 0
  while [i < (length container-size-cdf - 1)][
    set container-size-sum (first (item i container-size-cdf)) * (last (item i container-size-cdf) - lastval)
    set lastval (last (item i container-size-cdf))
    set i i + 1
  ]
  set all-hidden? false
  set wolbachia-hidden? true
  set roads no-patches

  update-fitness-costs

  set mosquito-mortality-rate-base 100 * (1 - exp(ln(1 - Mosquito-Mortality-Rate-Min / 100) / Ticks-Per-Day))
  set mosquito-mortality-rate-inc (100 * (1 - exp(ln(1 - Mosquito-Mortality-Rate-Max / 100) / Ticks-Per-Day)) - mosquito-mortality-rate-base) / Max-Mosquito-Age
  output-print (word "Mosquito mortality rates: base " precision mosquito-mortality-rate-base 3 " %/hour, increment " precision mosquito-mortality-rate-inc 3 " %/hour")
  set mosquito-mortality-rate-base-w 100 * (1 - exp(ln(1 - wolb-affect-adult-mortality-scf * Mosquito-Mortality-Rate-Min / 100) / Ticks-Per-Day))
  set mosquito-mortality-rate-inc-w (100 * (1 - exp(ln(1 - wolb-affect-adult-mortality-scf * Mosquito-Mortality-Rate-Max / 100) / Ticks-Per-Day)) - mosquito-mortality-rate-base-w) / Max-Mosquito-Age

  set SUSCEPTIBLE green
  set EXPOSED     yellow
  set INFECTIOUS  red
  set TEMPIMMUNE  magenta
  set RECOVERED   cyan
  set IMMUNED     blue
  ;; vector control
  set SPRAYS violet + 1
  set SPRAYE violet + 2
  set SPRAYI violet + 3

  set HOUSE gray - 3
  set WORK orange - 3
  set SCHOOL green - 3
  set OUTDOOR black
  set BORDER brown
  ;; vector control
  set HOUSE1 gray - 10
  set SPRAY violet
  set SCENTER yellow + 3
  set RCENTER yellow + 2
  set LCENTER yellow + 1
  
  set-default-shape people "person"
  set-default-shape mosquitos "hawk"
  set-default-shape germs "monster"
  set-default-shape males "hawk"
  ;; vector control
  set-default-shape sprayed "turtle"
  set-default-shape reductions "box"
  set-default-shape larvicidesprayed "bug"
  
  set all-germs []
  set continue-biting-prob (list 50 20 0) ;; probabilities that a mosquito will keep biting...
                                          ;; from Scott, et al. "Detection of Multiple Blood Feedings..." 1993.
  set finalized? false

  ;; array index pointers, increment in reverse, reset to (item 1 XXX-index)
  set egg-index array:from-list (list 0 (ticks-per-day * Egg-Hatch-Time - 1)) ;; index, max
  set larva-index array:from-list (list 0 (ticks-per-day * Eclose-Time - 1))  ;; index, max

  set travel-profile array:from-list n-values (25) [0]

  set migration-interval 12
  set migration-percent array:from-list n-values (42) [0]
  
  build-palette
    
  ;; sanity checks
  if (Initial-Person-Infection-Rate + Initial-Person-Immunity-Rate > 100) [
    output-print "Initial-Person-Infection-Rate + Initial-Person-Immunity must be <= 100 %"
    all-stop
  ]

  ;; vector control
  ;set is-vector-control 0
  ;set control-init-day 2
  ;set is-reduction-control 0
  ;set reduction-init-day 2
  ;set is-larvicide-control 0
  ;set larvicide-init-day 2
  ;set compliance-ratio 40
  ;set cover-radius 100
  ;set control-percent-eggs 60
  ;set control-percent-larvae 60
  ;set control-percent-mosquito 80
  set mortality-rate-mosquito-base 0.9
  set mortality-rate-mosquito-decay 0.083
  set mortality-rate-larvae-base 0.9
  set mortality-rate-larvae-decay 0.083
  ;;set effective-periods-mosquito 56
  ;set symptomatic-ratio 100
  set n-schedule 0
  set n-reduction 0
  set n-larvicide 0
  ;; seasonality
  ;set peak-day 0
  
  ;; now setup everything else
  set n-contact 0
  set n-mosquito 0
  set n-iv-contact 0
  ;set n-i-mosquito 0
  set n-ih-contact 0
  ;set n-i-human 0
  
  set host-incidence 0
  set vector-incidence 0
  
  setup-locations ;; must come before setup-agents
;  ask one-of (patches with [ovi-site?]) [set pverbose? true]
  setup-agents
  reset-bites
  ask turtles [set averbose? false]
  update-fields
  ;; now we must diffuse the oviposition site volatiles
  repeat Ovisite-Diffusion [diffuse wovi-dens 1.]

  my-setup-plots
  my-update-plots
  draw-patches
  
  set movie-on? false
  set setup-run-done? false
end

to reset-bites
  ;; nbites is an array of:
  ;; susceptible/infected vector bites susceptible/infected host
  ;; the four indices are: (sv->sh, iv->sh, sv->ih, iv->ih)
  set bite-matrix array:from-list n-values 4 [0]
end

to setup-infection-run
  let n count mosquitos with [wolbachia?]
  let m n-of (n * Wolb-Affect-Vectorial-Capacity / 100) (mosquitos with [wolbachia?])
  ask m [set vector-capable? false]

  no-display
  set view-on? false
  set plots-on? false
  set propagate-infection? true
  set infection-day-start (sim-day + 1)
  set max-days (sim-day + 120)
end

to increment-run-number
  ifelse behaviorspace-run-number = 0 [
    set run-number (run-number + 1)
  ][
    set run-number behaviorspace-run-number + run-number-start - 1
  ]
end

to-report get-state [c]
  if c = SUSCEPTIBLE [report "S"]
  if c = EXPOSED     [report "E"]
  if c = INFECTIOUS  [report "I"]
  if c = TEMPIMMUNE  [report "T"]
  if c = RECOVERED   [report "R"]
  if c = IMMUNED     [report "M"]
end

to print-diagnostic [m p message]
  output-print (word "m " [who] of m " " (get-state [color] of m) " " [itime] of m " : p " [who] of p " " (get-state [color] of p) " " [itime] of p " " message)
end

to print-agent [a message]
  let atype "M "
  if (is-person? a) [set atype "P "]
  output-print (word atype [who] of a " " (get-state [color] of a) " " [itime] of a " " message)
end

; generate a typical latency time( report in ticks)
to-report get-exposed-time
  let t (sample-from-rayleigh exposed-min exposed-max) * ticks-per-day
;output-print (word "returning exposed time = " t " (" (t / ticks-per-day) " days)")
  report t
end

; generate a typical infectious time( report in ticks)
to-report get-infectious-time
  let t (sample-from-rayleigh infectious-min infectious-max) * ticks-per-day
;output-print (word "returning infectious time = " t " (" (t / ticks-per-day) " days)")
  report t
end

; generate a typical eip time( report in ticks)
to-report get-eip-time
  let t (sample-from-rayleigh eip-min eip-max) * ticks-per-day
;output-print (word "returning EIP time = " t " (" (t / ticks-per-day) " days)")
  report t
end

to set-verbose
;  if (pxcor = 16 and pycor = 60) [set pverbose? true]
;  if (pxcor = 0 and pycor = 3) [set pverbose? true]

  ask one-of (patches with [ovi-site? and ovi-site-k = 10]) [set pverbose? true]
  ask one-of (patches with [ovi-site? and ovi-site-k = 50]) [set pverbose? true]
  ask one-of (patches with [ovi-site? and ovi-site-k = 100]) [set pverbose? true]
end

to-report get-larva-mortality-rate
  let s sigma * (1 + ((n-larvae + n-larvae-w) / ovi-site-k) ^ omega)
  if (s > 99) [set s 99]
;  if (pverbose?) [output-show (word "sigma = " precision sigma 1 ", n-larvae = " precision n-larvae 1 ", n-larvae-w = "
;    n-larvae-w ", ovi-site-k = " precision ovi-site-k 1 ", omega = " omega ", returning: " precision s 3)]
  ;test
  ;output-print (word "s: " s)
  report s
end


to-report factorial [val]
  set val round val
  if (val < 0) [report -1 * factorial (-1 * val)]
  if (val < 2) [report 1]
  report val * factorial (val - 1)
end

to-report binomial-prob [k n p]
  ;test
  ;output-print (word "k: " k " n:" n " p:" p)
  report (factorial n) / ((factorial k) * (factorial (n - k))) * (p ^ k) * ((1 - p) ^ (n - k))
end

to-report random-binomial [n p]
  ifelse (p = 1)[
    report n
  ][ 
    let x random-float 1
    let k 0
    let cdf binomial-prob k n p
    while [x > cdf and k < n] [
      set k k + 1
      set cdf cdf + binomial-prob k n p
    ]
    report k
  ]
end

to update-fitness-costs
  if (Use-Wolb-Affect-Fitness-Cost?) [
    set Wolb-Affect-Adult-Mortality-Rate Wolb-Affect-Fitness-Cost
    set Wolb-Affect-Fecundity            Wolb-Affect-Fitness-Cost
  ]
  set Wolb-Affect-Adult-Mortality-SCF (1 + (Wolb-Affect-Adult-Mortality-Rate / 100))
  set Wolb-Affect-Fecundity-SCF       (1 - (Wolb-Affect-Fecundity / 100))
  set Wolb-Affect-Mating-SCF          (1 - (Wolb-Affect-Mating-Probability / 100))
  set Wolb-Affect-Egg-SCF             (1 + (Wolb-Affect-Egg-Mortality-Rate / 100))
  set Wolb-Affect-Larva-SCF           (1 + (Wolb-Affect-Larva-Mortality-Rate / 100))
end

;; create initial populations
to setup-agents
  ;; must avoid wrapping, so let's throw away some border
  let active 1.0 ;; 0.95 ;; 80%
  let xwidth ((max-pxcor - min-pxcor) * active)
  let ywidth ((max-pycor - min-pycor) * active)

  ;; read the people from the file
  read-person-file

  ;; initial mosquitos have totally random locations
  let m (Initial-Mosquito-to-Human-Ratio * Initial-People)
  let n (m * Male-Mosquito-Ratio / 100)
  output-print (word "Creating " n " male mosquitos")
  create-males n [
    ;; give it an initial position
    setxy (random-float xwidth + min-pxcor + (1 - active) * xwidth / 2) (random-float ywidth + min-pycor + (1 - active) * ywidth / 2)
    ;; now set all the others...
    initialize-agent
    set age age-from-exponential Max-Mosquito-Age
  ]
  output-print (word "Creating " (m - n) " female mosquitos")
  create-mosquitos (m - n)[
    ;; give it an initial position
    setxy (random-float xwidth + min-pxcor + (1 - active) * xwidth / 2) (random-float ywidth + min-pycor + (1 - active) * ywidth / 2)
    ;; now set all the others...
    initialize-agent
    set age age-from-exponential Max-Mosquito-Age
    set eggs-remaining Eggs-Per-Brood * (1 - (age / Max-Mosquito-Age) * (Age-Dep-Fecundity-Reduction / 100)) / 10 ;; look ahead to laying time
;    set eggs-remaining 0
;output-show (word "eggs-remaining = " eggs-remaining)
    set body-clock (age mod Gonotrophic-Cycle-Length) - Gonotrophic-Cycle-Length; + ((random-float 2) - 1) ;; fully randomized...
    set hunger-clock (-1 * (random-float Time-Between-Bites ))
    set hungry? (body-clock < (-0.65 * Gonotrophic-Cycle-Length ))
    set bites (body-clock + Gonotrophic-Cycle-Length)
    if (bites < 0) [set bites 0]
    if (age > 1) [set mated? true]
  ]
  ;create-sprayed 1[
  ;  setxy 0 0
  ;  initialize-agent
  ;  set spray-day 1
  ;  set spray-sprout-number who 
  ;]  
 
  ;; since locations (and ovisites) were already created, we need to fill them with eggs/larvae
  let ovisites patches with [ovi-site?]
  let daily-eggs-per-k (((count mosquitos) * Eggs-Per-Brood / Gonotrophic-Cycle-Length) / sum [ovi-site-k] of ovisites) / 2
;  output-print (word "seeding " precision daily-eggs-per-k 1 " eggs per K")
  ask ovisites [
    set n daily-eggs-per-k * ovi-site-k
    let s 0
;    if (pverbose?) [output-show (word "n = " precision n 1 )]
    let i 0
    while [i < Egg-Hatch-Time] [
      let j 0
      while [j < Ticks-Per-Day] [
        array:set eggs (j + ticks-per-day * i) (n / Ticks-Per-Day)
        set j j + 1
      ]
      set s s + n
      set n n * (1 - Egg-Mortality-Rate / 100)
;      if (pverbose?) [output-print (word "eggs day " i ", n = " precision n 1 ", mu-max = " precision mu-max 3)]
      set i i + 1
    ]
    set n-larvae s ;; approximate, used to scale mu-max
    set n-larvae-w 0 ;; just to be sure
    set mu-max get-larva-mortality-rate / 100
;    if (pverbose?) [output-show (word "n-larvae = " precision n-larvae 1 ", n-larvae-w = " n-larvae-w ", ovi-site-k = " precision ovi-site-k 1 ", mu-max = " precision mu-max 3)]
    set i 0
    while [i < Eclose-Time] [
      let j 0
      while [j < Ticks-Per-Day] [
        array:set larvae (j + ticks-per-day * i) (n / Ticks-Per-Day)
        set j j + 1
      ]
;      if (pverbose?) [output-print (word "larva day " i ", n = " precision n 1 ", mu-max = " precision mu-max 3)]
;; age-dependent ;      set n n * (1 - ((mu-max / 100) * (Eclose-Time - i) / Eclose-Time))
      set n n * (1 - mu-max) ;; age-independent

;      if (pverbose?) [output-print (word "i3 = " i ", n = " n ", larva-mortality-rate-max = " mu-max)]
      set i i + 1
    ]
;    if (pverbose?) [output-print (word "length eggs = " length eggs ", sum eggs = " sum eggs)]
  ]
  
  ;; make some people immune...
  set n ((count people) * Initial-Person-Immunity-Rate / 100)
  output-print (word "Setting initial immunity in " n " people")
  ask n-of n people [
    set strain (random 4) + 1
    set first-strain strain
    while [strain = first-strain] [
      set first-strain (random 4) + 1
    ]
    set color IMMUNED
  ]
end

to-report age-from-exponential [maxage]
  let a (log (1. - (random-float 1)) 10) * -10
  while [a > maxage] [
    set a (log (1. - (random-float 1)) 10) * -10
  ]
  report a
end

to reload-infection-matrix
  ;; no infection sites -- useful for mosquito population studies
  let none "; target x y radius number percent strain\n"

  ;; infection point sources -- just a few
  let point-sources "; target x y radius number percent strain\n;[ 1 -30 -20 5 25 1]\n[ 1 -30  20 5 25 2]\n[ 1  30 -20 5 25 3]\n;[ 2  30  20 5 25 4]\n"

  ;; infections randomly distributed across the whole grid
  let uniform-random "; target x y radius number percent strain\n(list 1 0 0 1000 Initial-Mosquito-Infection-Rate 0)\n(list 2 0 0 1000 Initial-Person-Infection-Rate 0)\n"

  ;; select the requested one
  set infection-seed-matrix runresult Initial-Infection-Distribution
end

;; add mouse-clickable locations as infection sites
to add-infection-sites
  ;; use a timer, to avoid "holding down the button" syndrome...
  if (mouse-down? and (timer > 0.5)) [
    let x round mouse-xcor
    let y round mouse-ycor
    let tgt 1
    if (infection-seed-target = "human") [ set tgt 2 ]
    let infection-string (word "[" tgt " " x " " y " " infection-seed-radius " " infection-seed-percentage " " infection-seed-strain "]")

    ;; add this infection site to the infection seed matrix
    set infection-seed-matrix (word infection-seed-matrix infection-string "\n")
    output-print (word "new infection: " infection-string )
    reset-timer ;; to avoid multiple additions...
  ]
end

to color-patch
  if (mouse-down?)[ ; and (timer > 0.5)) [
    ask patch mouse-xcor mouse-ycor [
      set pcolor color-to-use
      display
    ]
;    reset-timer ;; to avoid multiple additions...
  ]
end

to infect-agents
  ;; !!! note:  We're still lacking initial infections in the "T" and "R" states...

  ;; load the infection seed sites (the germs) from the currently displayed matrix
  let matrix-string (word "set all-germs (list " infection-seed-matrix ")")
  run matrix-string

  output-print (word "Using infection seed matrix:\n" all-germs)
  output-print ""
  
  foreach n-values length all-germs [?] [ ;; each germ
    let g item ? all-germs

    ;; use this notion of a "germ" turtle breed because of the eventual ease of specifying other turtles "in-radius"
    create-germs 1 [
      set target (item 0 g)
      setxy (item 1 g) (item 2 g)
      set radius (item 3 g) / Meters-Per-Patch
      set number (item 4 g)
      set percent (item 5 g)
      set strain (item 6 g)
      set size 0
      set color white
      
      ;; first choose your target population
      let cloud nobody
      ifelse (1 = target) [
        output-print "DANGER WILL ROBINSON: Do not seed mosquito population, or 'infector' tracking may fail!!!"
        all-stop
        
        set cloud mosquitos in-radius radius
      ][
        set cloud people in-radius radius
      ]

      ;; now limit the target size by percentage
      if (0 = number) [
        set number ((percent / 100) * (count cloud))
      ]
      let n min (list number (count cloud with [color = SUSCEPTIBLE]))
      ifelse (n >= 1)[
        set cloud (n-of n cloud with [color = SUSCEPTIBLE]) ;; don't get the immunes...
      ][
        set cloud nobody
        output-print "ERROR: No Infected Agents!!!"
        all-stop
      ]
;;      output-print (word "INFO: germ target type " target ", serotype " strain " resulted in " (count cloud) " infected agents")
      if (show-infection-sources?)[
        set label n
        ifelse (target = 1) 
          [set label-color red + 3]
          [set label-color blue + 3]
      ]
      
      ;; infect the target sample
      ifelse (n >= 1)[
        output-print (word "INFO: infecting " (count cloud) " agents with serotype " strain)
        ask cloud [ ;; tell every agent to infect itself with this germ
          ;; make some infected people
          ifelse (is-person? self)[
            let ratio 100 * exposed-max / (exposed-max + infectious-max)
            ifelse (ratio > random 100)
              [ ;; exposed
                set itime ticks + get-exposed-time * (random-float 1) ;; randomize its start
                set color EXPOSED
              ][ ;; infectious
                set itime ticks + get-infectious-time * (random-float 1) ;; randomize its start
                set color INFECTIOUS
              ]
            set strain [strain] of myself
            if (strain = 0) [ set strain ((random 4) + 1)]
            set igen 0
            set infectedx xcor
            set infectedy ycor
            set infector -1
            set infectedx-1 0
            set infectedy-1 0
            set infector-1 -1
            ;; vector control
            if ((is-person? self) and (symptomatic-ratio >= random 100)) [
              set symptomatic 1
            ]
            log-infection
            if (is-person? self)[
              ;; vector control
              schedule-vector-control
              schedule-container-control
              schedule-larvicide-control
            ]
          ][
            output-print "ERROR: don't do this!!!"
            all-stop
          
            ;; make some infected mosquitos
            let ratio 70 ;; percent exposed vs. infectious
            ifelse (ratio > random 100) ;;
              [ ;; exposed
                set itime ticks + get-eip-time * (random-float 1) ;; randomize its start
                set color EXPOSED
              ][ ;; infectious
                set itime 0 ;; mosquitos never "recover", so infection transition time is NULL
                set color INFECTIOUS
              ]
            set strain [strain] of myself
            if (strain = 0) [ set strain ((random 4) + 1)]
            set infectedx xcor
            set infectedy ycor
          ]
        ]
      ][
        output-print (word "ERROR: No agents selected in infection cloud for serotype " strain)
        all-stop
      ]
    ]
  ]
end


to initialize-agent
  set hidden? all-hidden?
  set averbose? false
  set bornx xcor ;; store for later
  set borny ycor ;; store for later
  ifelse (is-mosquito? self)[ ;; mosquitos
    if (sim-day >= Infection-Day-Start) [
        set n-mosquito n-mosquito + 1
    ]
    set color SUSCEPTIBLE
    set age 0
    set age-max (Max-Mosquito-Age + (random-float 5) - 2)
    set body-clock (- Gonotrophic-Cycle-Length) + (random-float 2) - 1
    set bites 0
    set size 2
    set hunger-clock (-1 * Time-Between-Bites)
    set hungry? true
    set wolbachia? false
    set mated? false
    set sterile? false
    set strain 0
    set itime 0
    set strain 0
    set infectedx 0
    set infectedy 0
    set infector -1
    set infectedx-1 0
    set infectedy-1 0
    set infector-1 -1
    set igen -1
    set go-straight 0
    set eggs-remaining Eggs-Per-Brood
    set migrant? false
    set vector-capable? true
    if (not wolbachia-hidden? and wolbachia?) [set hidden? false]
    ;; vector control
    set spray-covered 0
    set own-sprayed-start-day 0
  ][ ifelse (is-male? self) [ ;; male mosquitos      
    set age 0
    set age-max (Max-Mosquito-Age + (random-float 5) - 2)
    set color gray
    set wolbachia? false
    set size 2
    set migrant? false
    set shape "hawk"
    if (not wolbachia-hidden? and wolbachia?) [set hidden? false]
    ;; vector control
    set spray-covered 0
    set own-sprayed-start-day 0
  ][ ifelse (is-person? self) [ ;; people
    set color SUSCEPTIBLE
    set first-strain 0
    set size 3
    set strain 0
    set itime 0
    set strain 0
    set infectedx 0
    set infectedy 0
    set infector -1
    set infectedx-1 0
    set infectedy-1 0
    set infector-1 -1
    set igen -1
    ;; vector control
    set symptomatic 0
  ][ ifelse (is-onesprayed? self) [ ;; vector control
    set color SCENTER
    set strain 0
    set itime 0
    set infectedx 0
    set infectedy 0
    set infector -1
    set infectedx-1 0
    set infectedy-1 0
    set infector-1 -1
    set age 0
    set igen -1
  ][ ifelse (is-reduction? self) [ ;; vector control
    set color RCENTER
    set strain 0
    set itime 0
    set infectedx 0
    set infectedy 0
    set infector -1
    set infectedx-1 0
    set infectedy-1 0
    set infector-1 -1
    set age 0
    set igen -1
  ][ ifelse (is-onelarvicidesprayed? self) [ ;; vector control
    set color LCENTER
    set strain 0
    set itime 0
    set infectedx 0
    set infectedy 0
    set infector -1
    set infectedx-1 0
    set infectedy-1 0
    set infector-1 -1
    set age 0
    set igen -1
  ][
    output-print "ERROR: unknown agent type!!!"
    output-show self
    all-stop
  ]]]]]]
end

to setup-locations
  ;; use the indices of these colors to describe building types
  ;let loc-colors (list HOUSE WORK SCHOOL OUTDOOR)
  let loc-colors (list HOUSE WORK SCHOOL OUTDOOR HOUSE1)

  ;; buildings
  read-location-file

  ;; roads
  if (Wolbachia-Release-On = "roads") [read-roads]

  ;; outdoor ovisites
  create-outdoor-ovisites
end

to create-outdoor-ovisites
  let iosites patches with [ovi-site?]
  let n-in-os (count iosites) ;; number of indoor ovisites
  let n-out-os n-in-os * Outdoor-Ovisite-Number-Ratio
  let oosites n-of n-out-os (patches with [bldg = OUTDOOR])
  ask oosites [
    init-ovisite false
  ]

  ; now renormalize all carrying capacities
  output-print (word "outdoor-ovisite-number-ratio " outdoor-ovisite-number-ratio)
  output-print (word "indoor-ovi-sites " n-in-os)
  output-print (word "outdoor-ovi-sites " n-out-os)
  
  let k-in sum [ovi-site-k] of iosites
  output-print (word "indoor-ovi-site-k-initial-avg " precision (k-in / n-in-os ) 3 )
  let k-out sum [ovi-site-k] of oosites
  ifelse (0 != n-out-os)
    [output-print (word "outdoor-ovi-site-k-initial-avg " precision (k-out / n-out-os ) 3 )]
    [output-print (word "outdoor-ovi-site-k-initial-avg 0")]
  let k-rat k-in / (k-in + k-out)
  ask patches with [ovi-site?] [set ovi-site-k ovi-site-k * k-rat]
  output-print (word "ovi-site-k-final-avg " precision (((sum [ovi-site-k] of iosites) + (sum [ovi-site-k] of oosites)) / (n-in-os + n-out-os)) 3 )
end

to my-setup-plots
  set-current-plot "Mosquito Ages"
  set-histogram-num-bars 25
  
  set-current-plot "Mosquito Travel"
  set-plot-x-range 0 500
  set-histogram-num-bars 25

  set-current-plot "Travel Profile"
  auto-plot-on
  
  set-current-plot "Wolbachia Penetration"
  auto-plot-on

  set-current-plot "Bites"
  set-plot-x-range 0 5
  set-histogram-num-bars 5
  
  set-current-plot "SEI - Mosquitos"
  set-plot-y-range 0 0.01
;  auto-plot-on

  set-current-plot "SEITRM - People"
  set-plot-y-range 0 0.01
;  auto-plot-on

  set-current-plot "Percentages"
  set-plot-y-range 0 100.
end

to setup-files
  foreach [".ilog" ".perf" ".summary" ".perf-head" ".ilog-head"] [
    let fname (word Run-Name "-" run-number ?)
    if file-exists? fname [file-delete fname]
  ]

  write-summary "initial"

  if Header-Files? [
;    file-open (word Run-Name "/out-" run-number ".perf-head")
;    file-print "ticks count-people count-mosquitos sim-timer (sim-timer)-(perf-timer)"
;    file-close
  
    file-open (word Run-Name "/out-" run-number ".ilog-head")
    file-print "igen agent(type ID infx infy bornx borny) infector(ID infx infy infector-ID) sim-day"
    file-close
  ]
end

to write-summary [stage]
  if (write-data?)[
  ifelse (stage = "initial")[
    let oosites patches with [ovi-site? and bldg = OUTDOOR]
    let iosites patches with [ovi-site? and bldg != OUTDOOR]
    
    file-open (word Run-Name "/out-" run-number ".summary")
    file-print (word "Run-Number " Run-Number)
    file-print (word "Run-Name: " Run-Name)
    file-print "Initial"
    file-print (word "Input-File-Root '" Input-File-Root "'")
    file-print (word "count-people " count people)
    file-print (word "count-mosquitos " count mosquitos)
    file-print (word "outdoor-ovisite-number-ratio " outdoor-ovisite-number-ratio)
    file-print (word "indoor-ovi-sites " count iosites)
    file-print (word "outdoor-ovi-sites " count oosites)
    file-print (word "ovi-site-k-final-avg " precision (((sum [ovi-site-k] of iosites) + (sum [ovi-site-k] of oosites)) / (count iosites + count oosites)) 3 )
    file-close
  ][ ifelse (stage = "equilibrium") [
    file-open (word Run-Name "/out-" run-number ".summary")
    file-print "Equilibrium"
    file-print (word "sim-day " (precision sim-day 3))
    file-print (word "count-mosquitos " count mosquitos)
    file-print (word "total-mosquitos " n-mosquito)
    file-print (word "total-contact " n-contact)
    file-print (word "expect-contact per day per mosquito " precision (n-contact / (n-mosquito * (sim-day - Infection-Day-Start))) 3)
    ;file-print (word "total-infected-mosquitos " n-i-mosquito)
    file-print (word "total-infected-mosquitos-contact " n-iv-contact)
    file-print (word "expect-contact of infected mosquitos per day " precision (n-iv-contact / (sim-day - Infection-Day-Start)) 3)
    ;file-print (word "total-infected-humans " n-i-human)
    file-print (word "total-infected-humans-contact " n-ih-contact)
    file-print (word "expect-contact of infected humans per day " precision (n-ih-contact / (sim-day - Infection-Day-Start)) 3)
    file-print (word "count-males " count males)
    file-print (word "migrant-num-mean " precision (mean array:to-list migration-percent) 3)
    file-print (word "migrant-num-stdev " precision (standard-deviation array:to-list migration-percent) 3)    
    file-print (word "females-per-village-property " precision ((count mosquitos-on village-patches) / nproperties) 3)
    file-print (word "sim-timer " (precision sim-timer 3))
    file-close
  ][ ifelse (stage = "final") [
    ;let bset patches with [bldg = HOUSE]
    let bset patches with [(bldg = HOUSE) OR (bldg = HOUSE1)]
    let brat (count patches) / (count bset)
    let fmhrat (count mosquitos) / (count mosquitos-on bset)

    file-open (word Run-Name "/out-" run-number ".summary")
    file-print "Final"
    file-print (word "sim-day " (precision sim-day 3))
    file-print (word "Random-Seed-Value " Random-Seed-Value)
    file-print (word "count-people " count people)
    file-print (word "count-mosquitos " count mosquitos)
    file-print (word "total-mosquitos " n-mosquito)
    file-print (word "total-contact " n-contact)
    file-print (word "expect-contact per day per mosquito " precision (n-contact / (n-mosquito * sim-day)) 3)
    ;file-print (word "total-infected-mosquitos " n-i-mosquito)
    file-print (word "total-infected-mosquitos-contact " n-iv-contact)
    file-print (word "expect-contact of infected mosquitos per day " precision (n-iv-contact / (sim-day - Infection-Day-Start)) 3)
    ;file-print (word "total-infected-humans " n-i-human)
    file-print (word "total-infected-humans-contact " n-ih-contact)
    file-print (word "expect-contact of infected humans per day " precision (n-ih-contact / (sim-day - Infection-Day-Start)) 3)
    file-print (word "count-males " count males)
    file-print (word "unfertilized-females " count mosquitos with [not mated?])
    ifelse (count mosquitos with [not mated?] > 0)[
      file-print (word "mean-age-unfertilized-females " (precision mean [age] of mosquitos with [not mated?] 3))
      file-print (word "median-age-unfertilized-females " (precision median [age] of mosquitos with [not mated?] 3))
      file-print (word "max-age-unfertilized-females " (precision max [age] of mosquitos with [not mated?] 3))
    ][
      file-print "mean-age-unfertilized-females 0"
      file-print "median-age-unfertilized-females 0"
      file-print "max-age-unfertilized-females 0"
    ]
    file-print (word "migrant-num-mean " precision (mean array:to-list migration-percent) 3)
    file-print (word "migrant-num-stdev " precision (standard-deviation array:to-list migration-percent) 3)    
    file-print (word "all-to-building-patch-ratio " precision brat 3 )
    file-print (word "all-to-building-mosquito-ratio " precision fmhrat 3)
    file-print (word "k-per-house " (k-per-house * inflate-k))
    file-print (word "females-per-village-property " precision ((count mosquitos-on village-patches) / nproperties) 3)
    file-print (word "sim-timer " (precision sim-timer 3))
    file-close
  ][
    output-print (word "Invalid summary stage: " stage)
    all-stop
  ]]]
  ]
end

to build-palette
  set palette [0]
  foreach n-values 10 [?] [
    let h ((9 - ?) * 24 - 20)
    let col approximate-hsb h 255 (100 + 16 * ?)
    set palette lput col palette
  ]
end

;; model propagation -------------------------------------------------------------
to Go
  if not setup-run-done? [setup-run] ;; do this now in order to get per-run customizations set by BehaviorSpace
  
  reset-timer
    
  if (sim-day >= Max-Days) [ stop ]          ;; time's up -- no all-stop, because this may be a transient (interactive) condition
  if (not any? mosquitos)  [ stop all-stop ] ;; mosquitos all dead
  if (not any? people)     [ stop all-stop ] ;; people all dead
  if (Finalized?)          [ stop all-stop ] ;; someone else must have shut it down
  if (Propagate-Infection? and (sim-day > Infection-Day-Start) and               ;; after infections injected
      (0 = count mosquitos with [(color = EXPOSED) or (color = INFECTIOUS)]) and ;; no more infectious mosquitos
      (0 = count people with [(color = EXPOSED) or (color = INFECTIOUS)]))       ;; no more infectious people
    [stop all-stop]   ;; nothing left to run for
  
  move-agents     ;; all movement
  mate-mosquitos  ;; males look for females
  bite-people     ;; all bites
  update-time     ;; includes progressing infection states
  update-fields   ;; update the host/mosquito density fields, and other weighted fields
  update-graphics ;; all the graphics and GUI-related stuff here
  
  if write-data? [write-data]
  
  if (0 = time-of-day and (round sim-day = Equilibrium-Day))[
    write-summary "equilibrium"
  ]
    
  set sim-timer (sim-timer + timer)
;  if (0 = time-of-day)[
;    update-perf-timer
;  ]
end

to setup-run
  if (not setup-run-done?)[
    setup-files
    set setup-run-done? true
  ]
end

to fix-mosquitos
  let broken males with [size = 1]
  output-print (word "sim-day " sim-day ": random males " (count broken))
  ask broken [
    initialize-agent
  ]
end

to mate-mosquitos
  ask males [
    let p Mating-Probability
    if (wolbachia?) [set p p * Wolb-Affect-Mating-SCF]
    if (random 100 < p) [                                  ; don't waste your time if it's not meant to be...
      let femme one-of mosquitos in-radius Mating-Radius   ; pick a female in range, allow females to be multi-mated... but ignore the results of non-primaries
      if (femme != nobody)[
        if (not ([mated?] of femme)) [ ; if this is not the first, then disregard the results
          ask femme [set mated? true]                        ; no more mating after this.
          if (Wolbachia? and (not [Wolbachia?] of femme))[   ; in just this one case...
            ask femme [set sterile? true]                    ; ... the female becomes sterile due to cytoplasmic incompatibility
          ]
        ]
      ]
    ]
  ]
end

to update-graphics
  if view-on? [
    draw-patches  ;; must do this each step, if what's plotted varies each step
  ]
  
  if plots-on? [
    my-update-plots  ;; do the numerics
  ]
  
  if (movie-on?) [
    movie-grab-view       ;; just the model world (the grid)
                          ;    movie-grab-interface ;; the ENTIRE Interface tab/screen
    output-print movie-status 
  ]
end

to update-perf-timer
  file-open (word Run-Name "/out-" run-number ".perf")
  ;; NOTE: If you change this line, be sure to update the header in setup-files
  file-print (word ticks " " ((count people) + (count mosquitos)) " " (precision sim-timer 3) " " (precision (sim-timer - perf-timer) 3))
  set perf-timer sim-timer
end

to start-movie
    movie-start (word Run-Name "-" run-number ".mov")
;    movie-set-frame-rate 10
    set movie-on? true
end

to stop-movie
  if (movie-on?) [
    output-print movie-status
    movie-close
  ]
  set movie-on? false
end

to all-stop
;  update-perf-timer ;; make sure to write out the end state
  finalize-files
  
;  stop-movie
  file-close-all
  stop
end

to write-data
  let ovisites village-patches with [ovi-site?]
  let fm mosquitos-on village-patches
  let mm males-on village-patches

;; only want to consider village squares here...
  if write-pop-file? [
    file-open (word Run-Name "/out-" run-number ".pop")
    file-write precision sim-day 3
    file-write precision ((sim-day - Wolbachia-Release-Start-Day) / 7) 3
    file-write count fm
    file-write precision (100 * count fm with [wolbachia?] / count fm) 2
    file-write precision (100 * (sum [n-eggs-w] of ovisites) / (sum [n-eggs + n-eggs-w] of ovisites)) 2
    file-write precision (100 * (sum [n-larvae-w] of ovisites) / (sum [n-larvae + n-larvae-w] of ovisites)) 2
    file-write precision (100 * count fm with [sterile?] / count fm) 2
    file-print " "
    file-close
  ]
  
;; write the bite file
  if write-bite-file? [
    file-open (word Run-Name "/out-" run-number ".bite")
    file-write precision sim-day 3
    file-write array:item bite-matrix 0
    file-write array:item bite-matrix 1
    file-write array:item bite-matrix 2
    file-write array:item bite-matrix 3
    file-write count ((mosquitos-on village-patches) with [SUSCEPTIBLE = color])
    file-write count ((mosquitos-on village-patches) with [EXPOSED = color])
    file-write count ((mosquitos-on village-patches) with [INFECTIOUS = color])
    file-write count (people with [SUSCEPTIBLE = color])
    file-write count (people with [EXPOSED = color])
    file-write count (people with [INFECTIOUS = color])
    file-print " "
    file-close
  ]
  
  if write-mate-file? [
    ;; same, but now the mating file
    file-open (word Run-Name "/out-" run-number ".mate")
    file-write count fm
    file-write count mm
    file-write count fm with [not mated?]
    ifelse (count fm with [not mated?] > 0)[
      file-write (precision mean [age] of fm with [not mated?] 3)
      file-write (precision median [age] of fm with [not mated?] 3)
      file-write (precision max [age] of fm with [not mated?] 3)
    ][
      file-write "0.000"
      file-write "0.000"
      file-write "0.000"
    ]
    file-print ""
    file-close
  ]
  
  if write-mig-file? [
    ;; must consider whole grid here...
    file-open (word Run-Name "/out-" run-number ".mig")
    file-write precision sim-day 3
    file-write precision ((sim-day) / 7) 3
    file-write count mosquitos
    file-write precision (100 * count mosquitos with [migrant?] / count mosquitos) 2
    file-print " "
    file-close  
  ]
  
  if propagate-infection? and (sim-day >= infection-day-start) and (0 = time-of-day) [
    file-open (word Run-Name "/out-" run-number ".seir")
    file-write sim-day
    file-write count people
    file-write count (people with [color = SUSCEPTIBLE])    
    file-write count (people with [color = EXPOSED])
    file-write count (people with [color = INFECTIOUS])    
    file-write count (people with [color = TEMPIMMUNE])    
    file-write count (people with [color = RECOVERED])    
    file-write count (people with [color = IMMUNED])    
    file-write host-incidence
    set host-incidence 0

    file-write count mosquitos
    file-write count (mosquitos with [color = SUSCEPTIBLE])
    file-write count (mosquitos with [color = EXPOSED])
    file-write count (mosquitos with [color = INFECTIOUS])    
    file-write vector-incidence
    set vector-incidence 0
    file-print " "
    file-close
  ]
end

to write-input-file
  let fname (word Run-Name "/input-" Behaviorspace-Run-Number ".cmd")
  if file-exists? fname [file-delete fname]
  file-open fname
  file-print (word "set run-name \"" run-name "\"")
  file-print (word "set run-number " (behaviorspace-run-number + run-number-start - 1))
  file-print (word "random-seed " random-seed-value)
  file-print (word "set random-seed-value " random-seed-value)
  file-print (word "set wolb-affect-vectorial-capacity " wolb-affect-vectorial-capacity)
  file-print (word "set Initial-Person-Immunity-Rate " Initial-Person-Immunity-Rate)
  file-print (word "set V-to-H-Transmissibility " V-to-H-Transmissibility)
  file-print (word "set H-to-V-Transmissibility " H-to-V-Transmissibility)

  let ipeeps ((count people) * Initial-Person-Immunity-Rate / 100)
  file-print (word "ask n-of " ipeeps " people [set color IMMUNED]")

  file-close  
end

to read-input-file
  let fname (word Run-Name "/input-" Behaviorspace-Run-Number ".cmd")
  if not file-exists? fname [
    output-print (word "cannot open " fname ": no input file found.")
    all-stop
  ]
  let line ""
  file-open fname
  while [not file-at-end?] [
    set line file-read-line
;    print line
    run line
  ]
  file-close
end

to write-travel-profile
; with error bars
  file-open (word Run-Name "/out-" run-number ".tprof")

  let i 0
  while [i < array:length travel-profile] [
    let mos mosquitos with [(not migrant?) and (age >= i) and (age < i + 1)]
    let avg 0
    let err 0
    if any? mos [
      set avg (Meters-Per-Patch * mean [sqrt((xcor - bornx) ^ 2 + (ycor - borny) ^ 2)] of mos)
      ifelse (1 < count mos)
      [ set err (Meters-Per-Patch * standard-deviation [sqrt((xcor - bornx) ^ 2 + (ycor - borny) ^ 2)] of mos) / sqrt (count mos)]
      [ set err avg ]
    ]

    file-print (word i " " avg " " err)

    set i i + 1
  ]

  file-close
end

to dump-ovi-dens
  let fname (word Run-Name "/ovi-dens.dat")
  if file-exists? fname [file-delete fname]
  file-open fname
  ask patches with [ovi-site?][
    file-print (word pxcor " " pycor " " ovi-site-k " " ovi-dens)
  ]
  file-close
end

to dump-larvae
  let fname (word Run-Name "/larvae.dat")
  if file-exists? fname [file-delete fname]
  file-open fname
  ask patches with [ovi-site?][
;    file-print (word pxcor " " pycor " " array:to-list larvae-w)
    file-print array:to-list larvae
  ]
  file-close
end

to dump-eggs
  let fname (word Run-Name "/eggs.dat")
  if file-exists? fname [file-delete fname]
  file-open fname
  ask patches with [ovi-site?][
    file-print array:to-list eggs
  ]
  file-close
end

to finalize-files
  if (not finalized?)[
    if write-data? and write-tprof-file? [write-travel-profile]
    write-summary "final"    
    set finalized? true
  ]
end

;; all agent motion
to move-agents
  ask mosquitos [
    let oldx xcor
    let oldy ycor
    take-mosquito-step ;; there's a lot rolled into this...

    if ((oldx * xcor < 0) and (abs xcor > max-pxcor / 2)) or ((oldy * ycor < 0) and (abs ycor > max-pycor / 2))[
      set migrant? true
      if Clear-Wolbachia-On-Migration? [set wolbachia? false]
    ]
  ]

  ask males [
    let oldx xcor
    let oldy ycor
    ;; males just look for a shady spot and wait to ambush females
    take-endophilic-step Mosquito-House-Affinity Mosquito-Migration-Step
    if ((oldx * xcor < 0) and (abs xcor > max-pxcor / 2)) or ((oldy * ycor < 0) and (abs ycor > max-pycor / 2))[
      set migrant? true
      set wolbachia? false
    ]
  ]
  
  ask people [
    if (1 = time-of-day) [   ;; go to work/school (which could also be "home")
      setxy wrkx wrky
    ]
    if (8 = time-of-day) [                      ;; go home
       setxy bornx borny
    ]
    if (11 = time-of-day or 17 = time-of-day) [ ;; go back home
       setxy bornx borny
    ]
        
    if (is-day?) [ ; Day time... wander around workplace/school
      take-endophilic-step Person-Building-Affinity Person-Migration-Step
    ]
  ]
end

to take-mosquito-step
  ifelse (hungry? and (hunger-clock > 0)) [
    ifelse Wiggle-On?
      [take-wiggle-step Mosquito-Migration-Step 1]   ;; host-seeking
      [take-gradient-step Mosquito-Migration-Step 1]
  ][ ifelse (body-clock > 0) [
    ifelse (go-straight > 0)[ ;; already laid partial, must find another
      forward Mosquito-Migration-Step
      set go-straight go-straight - Mosquito-Migration-Speed ;; in meters, not grid spacing
      if (go-straight < 0) [set go-straight 0]
    ][ 
      ifelse Wiggle-On?
        [take-wiggle-step Mosquito-Migration-Step 2]   ;; ovisite-seeking
        [take-gradient-step Mosquito-Migration-Step 2]
    ]
  ][
    take-endophilic-step Mosquito-House-Affinity Mosquito-Migration-Step ;; digesting
  ]]
end

to take-endophilic-step [affinity stepsize]
  let attempt 10 ;; needs multiple tries to avoid problems...
  while [attempt > 0] [
    right (random 360)
    let p patch-ahead stepsize
    if (nobody != p) [
      let b [bldg] of p
      ifelse (OUTDOOR != b) and (BORDER != b) [
        ;; inside a building... take it
        set attempt 0
      ][ ;; outside the house... MAYBE take it
        ifelse (random 100 > affinity) [
          set attempt 0   ;; less than ideal, but take it with some non-zero probability
        ][
          set attempt (attempt - 1)  ;; try for another
        ]
      ]
    ]
  ]
  forward stepsize
end

to take-shady-step [affinity stepsize]
  ;; !!! highly problematic, since in-cone references patch CENTERS, not just patch boundaries
  let cone (patches in-cone stepsize Wiggle-Size)
  let tgt no-patches
  ifelse (random-float 100 > affinity) [
    set tgt one-of cone             ;; go anywhere in the forward cone
  ][ 
  set tgt max-one-of cone [shade] ;; just pick the shadiest spot
  ]
  face tgt
  setxy ([pxcor] of tgt + (random-float 1) - 0.5) ([pycor] of tgt + (random-float 1) - 0.5)
end

to take-gradient-step [stepsize whichfield]
  let p patch 0 0
  ifelse (whichfield = 1) [   ;; field 1 = host volatiles
    set p max-one-of neighbors [host-dens]
    if [host-dens] of p < host-dens [set p one-of neighbors] ;; if nobody is better, just go /somewhere/...
  ][                          ;; field 2 = ovisite volatiles
    set p max-one-of neighbors [wovi-dens]
    if [wovi-dens] of p < wovi-dens [set p one-of neighbors] ;; if nobody is better, just go /somewhere/...
  ]
  
  face p
  forward stepsize
end

to take-wiggle-step [stepsize whichfield]
;; !!! consider using 'patches in-cone dist angle' !!!
  let scent-ahead field-at-angle   0                stepsize whichfield
  let scent-right field-at-angle  Wiggle-Size       stepsize whichfield
  let scent-left  field-at-angle (-1 * Wiggle-Size) stepsize whichfield

  if (scent-right > scent-ahead) or (scent-left > scent-ahead)[
    ifelse scent-right > scent-left 
      [ rt Wiggle-Size ]
      [ lt Wiggle-Size ]
  ]
  
  forward stepsize
end

to-report field-at-angle [angle dist whichfield]
  let p patch-right-and-ahead angle dist
  if p = nobody [ report 0 ]
  ifelse (whichfield = 1) 
    [report [host-dens] of p] ;; field 1 = host volatiles
    [report [wovi-dens] of p] ;; field 2 = ovisite volatiles
end

to update-fields
  if (0 = (ticks mod migration-interval)) [
    array:set migration-percent ((ticks / migration-interval) mod array:length migration-percent) (100 * (count mosquitos with [migrant?]) / (count mosquitos))
  ]
  
  ;; add to the fields
  ask people [
    set host-dens (host-dens + 1)
  ]
  ask mosquitos [
    set mosq-dens (mosq-dens + 1)
  ]

  let threshold 0.15 ;; field is considered gone when it reaches this level...
  let e-rate threshold ^ (1 / Evaporation-Time)

  ask patches [
    if (ovi-site?)[
      ;; !!! fix this, because it's wasteful of memory, and maybe performance too...
      set n-eggs sum array:to-list eggs
      set n-eggs-w sum array:to-list eggs-w
      set n-larvae sum array:to-list larvae
      set n-larvae-w sum array:to-list larvae-w

;; don't use actual density for attraction, but just number of larvae
;      set ovi-dens (n-larvae + n-larvae-w) / ovi-site-k ;; scale it up, so we're counting realistic larvae
      set ovi-dens (n-larvae + n-larvae-w) ;; scale it up, so we're counting realistic larvae
      set wovi-dens wovi-dens + (ovi-dens * ovi-scf)
;      if (pverbose?)[ output-show (word "n-eggs = " n-eggs ", n-eggs-w = " n-eggs-w ", n-larvae = " n-larvae ", n-larvae-w = " n-larvae-w ", ovi-dens = " ovi-dens) output-show larvae   ]
    ]
    
    ;; subtract from the fields (evaporation-time)
    set host-dens host-dens * e-rate
    set mosq-dens mosq-dens * e-rate
    set wovi-dens wovi-dens * e-rate

    if ("wolb" = substring Patch-Color-Shows 0 4)[
      if (view-on? and 0 = pxcor mod Wolbachia-Patch-Aggregation and 0 = pycor mod Wolbachia-Patch-Aggregation) [ ;; only "lead" patches do this...
        let minx pxcor
        let miny pycor
        let maxx min (list (minx + (Wolbachia-Patch-Aggregation - 1)) max-pxcor)
        let maxy min (list (miny + (Wolbachia-Patch-Aggregation - 1)) max-pycor)
        let quad patches with [pxcor >= minx and pxcor <= maxx and pycor >= miny and pycor <= maxy]
        
        let w 0
        ifelse (last Patch-Color-Shows = "f") [
          let m count mosquitos-on quad
          if (m > 0) [set w (count (mosquitos-on quad) with [wolbachia?]) / m]
        ][ ifelse (last Patch-Color-Shows = "l") [
          let l sum [n-larvae + n-larvae-w] of quad
          if (l > 0) [set w (sum [n-larvae-w] of quad) / l]
        ][ ifelse (last Patch-Color-Shows = "e") [
          let ee sum [n-eggs + n-eggs-w] of quad
          if (ee > 0)  [set w (sum [n-eggs-w] of quad) / ee]
        ][ ifelse (last Patch-Color-Shows = "a") [
          let a count mosquitos-on quad + count males-on quad + sum [n-larvae + n-larvae-w] of quad + sum [n-eggs + n-eggs-w] of quad
          if (a > 0)  [set w (count (mosquitos-on quad) with [wolbachia?] + count (males-on quad) with [wolbachia?] + sum [n-larvae-w] of quad + sum [n-eggs-w] of quad) / a]
        ][
          output-print (word "unrecognized patch request: " Patch-Color-Shows)
        ]]]]

        ask quad [set wolb-dens w] ;; give the aggregated value back to all
      ]
    ]
  ]

  ;; apply diffusion
  repeat 2 [diffuse host-dens Diffusion-Rate / 100]
  repeat 2 [diffuse mosq-dens Diffusion-Rate / 100]
  repeat 2 [diffuse wovi-dens Diffusion-Rate / 100]
end

to colorize
  ;; multiple-use of the wolbachia patch field
;  ask patches with [0 = pxcor mod Wolbachia-Patch-Aggregation and 0 = pycor mod Wolbachia-Patch-Aggregation] [
  ask patches with [0 = pxcor and 0 = pycor] [
    let minx pxcor
    let miny pycor
    let maxx min (list (minx + (Wolbachia-Patch-Aggregation - 1)) max-pxcor)
    let maxy min (list (miny + (Wolbachia-Patch-Aggregation - 1)) max-pycor)
    let quad patches with [pxcor >= minx and pxcor <= maxx and pycor >= miny and pycor <= maxy]

    ask quad [set pcolor white]
  ]
end

;; here's where the biting/infecting happens -----------------------------------------------------------
to bite-people
  if feeding-time? [
    ask mosquitos with [hungry? and (hunger-clock > 0)] [
      let victim one-of (people in-radius (Bite-Radius / Meters-Per-Patch)) ;; choose your victim
      if (victim != nobody) and (random 100 < Bite-Probability) [ ;; bite
;        output-print "biting!"
        if (sim-day >= Infection-Day-Start) [
            set n-contact n-contact + 1
        ]
        if (random 100 >= item bites continue-biting-prob) [ set hungry? false]

        set bites (bites + 1) ;; counter for bites by this mosquito
        
        ;; increment the SI/VH bite matrix
        ;; note that we roll EXPOSED into the SUSCEPTIBLE counter, and ignore RECOVERED... thus multiple serotypes
        ifelse ((SUSCEPTIBLE = color or EXPOSED = color) and (SUSCEPTIBLE = [color] of victim or EXPOSED = [color] of victim))[
          array:set bite-matrix 0 ((array:item bite-matrix 0) + 1)
        ][ ifelse (INFECTIOUS = color and (SUSCEPTIBLE = [color] of victim or EXPOSED = [color] of victim))[
          array:set bite-matrix 1 ((array:item bite-matrix 1) + 1)
        ][ ifelse ((SUSCEPTIBLE = color or EXPOSED = color) and INFECTIOUS = [color] of victim)[
          array:set bite-matrix 2 ((array:item bite-matrix 2) + 1)
        ][ ifelse (INFECTIOUS = color and INFECTIOUS = [color] of victim)[
          array:set bite-matrix 3 ((array:item bite-matrix 3) + 1)
        ][
          ; no-op, don't care
        ]]]]
        
        ;; propagate infections (both directions) if appropriate
        infect victim
      ]
    ]
  ]
end

to-report feeding-time?
  report (is-day?)
end

;; run by a biting mosquito
to infect [victim]
  ;; mosquito infects victim
  ifelse (((INFECTIOUS = color) or (SPRAYI = color))                             ;; mosquito is infectious including vector control
    and ((SUSCEPTIBLE = [color] of victim)             ;; victim is susceptible
      or ((RECOVERED = [color] of victim)              ;; victim is semi-susceptible
        and (strain != [first-strain] of victim)))) ;; victim has not been exposed to this strain before
  [
    ifelse (random-float 1 < V-to-H-Transmissibility) [
      set n-iv-contact n-iv-contact + 1
      ;set n-i-human n-i-human + 1
;print-diagnostic self victim "infecting person"
      ask victim [
        set itime ticks + get-exposed-time
        set strain ([strain] of myself)
        set color EXPOSED
        set host-incidence (host-incidence + 1)

        set infector [who] of myself
        set infectedx xcor
        set infectedy ycor

        set infector-1 [infector] of myself
        set infectedx-1 [infectedx] of myself
        set infectedy-1 [infectedy] of myself
        
        create-link-from person infector-1

        set igen (([igen] of myself) + 1)

        ;; vector control
        if (symptomatic-ratio >= random 100) [
          set symptomatic 1
        ]

        log-infection
        ;; vector control
        ;; this is an explicilty human context, so we don't have to check is-person? ...
        schedule-vector-control
        schedule-container-control
        schedule-larvicide-control
      ]
    ][
;      output-print "NOT infecting host, below transmissibility"
    ]
  ][
    ;; victim infects mosquito
    ifelse (((SUSCEPTIBLE = color) or (SPRAYS = color))             ;; mosquito is susceptible (this forces a single-strain vector) including vector control
      and (INFECTIOUS = [color] of victim)    ;; victim is infectious
      and (vector-capable?))                  ;; mosquito is vector-capable
    [
      ifelse (random-float 1 < H-to-V-Transmissibility) [
        set n-ih-contact n-ih-contact + 1
        ;set n-i-mosquito n-i-mosquito + 1
;print-diagnostic self victim "infecting mosquito"
        set infector [who] of victim
        set itime ticks + get-eip-time
        set strain [strain] of victim
        ifelse (SUSCEPTIBLE = color) [
          set color EXPOSED
        ][
          set color SPRAYE
        ]  
        set vector-incidence (vector-incidence + 1)

        set infector [who] of victim
        set infectedx xcor
        set infectedy ycor

        set infector-1 [infector] of victim
        set infectedx-1 [infectedx] of victim
        set infectedy-1 [infectedy] of victim

        set igen (([igen] of victim) + 1)
;print-diagnostic self victim "mosquito infected"
        log-infection
      ][
;        output-print "NOT infecting vector, below transmissibility"
      ]
    ][
;print-diagnostic self victim "no new infections"
    ]
  ]
end

to select-wolbachia-sites
  let relnum (round sim-day - Wolbachia-Release-Start-Day) / Wolbachia-Release-Frequency

  if (0 = relnum) [
    apply-larval-suppression
    apply-adult-suppression
  ]
  
  ifelse (Wolbachia-Release-On = "ovi-sites")[
    let ovisites patches with [ovi-site?]
    set wolbachia-sites n-of ((count ovisites) / (Wolbachia-Sites-Every-Nth * Breteau-Index / 100)) ovisites ;; at random, for now...
  ][ ifelse (Wolbachia-Release-On = "front-doors")[
    set wolbachia-sites patches with [front-door-id != 0 and (front-door-id mod Wolbachia-Sites-Every-Nth) = (relnum mod Wolbachia-Sites-Every-Nth)]
  ][ ifelse (Wolbachia-Release-On = "roads")[
    set wolbachia-sites patches with [is-road?]
  ][
    output-print (word "Unimplemented Wolbachia-Release-On value: " Wolbachia-Release-On)
    all-stop
  ]]]
  output-type (word "Wolbachia release " relnum " on " Wolbachia-Release-On ", day " round sim-day ": using " (count wolbachia-sites) " wolbachia sites")
end

to apply-adult-suppression
  let nkill ((count males) * Wolb-Pre-Release-Adult-Suppression / 100)
  output-print (word "Killing " nkill " male mosquitos")
  ask n-of nkill males [die]
  set nkill ((count mosquitos) * Wolb-Pre-Release-Adult-Suppression / 100)
  output-print (word "Killing " nkill " female mosquitos")
  ask n-of nkill mosquitos [die]
end

to apply-larval-suppression
  let redux (Wolb-Pre-Release-Larval-Suppression / 100)
  let ovisites (patches with [ovi-site?])
  let tgtsites n-of (redux * (count ovisites)) ovisites  
  output-print (word "wiping out " (count tgtsites) " of " (count ovisites) " ovisites")
  ask tgtsites [
    set larvae array:from-list n-values (ticks-per-day * Eclose-Time) [0]
    set larvae-w array:from-list n-values (ticks-per-day * Eclose-Time) [0]
  ]
end

to inject-wolbachia
  ;; choose a new set of sites each time
  select-wolbachia-sites

  let nmos Wolbachia-Seed-Adults-per-house  * nhouses * (Wolbachia-Release-Frequency / 7)
  let nm nmos * (Male-Mosquito-Ratio / 100)
  output-print (word "day " sim-day ": creating " nm " male mosquitos with wolbachia")
  ;; inject NEW mosquitos into the model
  create-males nm [
    let site one-of wolbachia-sites
    setxy [pxcor] of site [pycor] of site
    initialize-agent
    set wolbachia? true
    if (not wolbachia-hidden?) [set hidden? false]
  ]
  let nf (nmos - nm)
  output-print (word "day " sim-day ": creating " nf " female mosquitos with wolbachia")
  create-mosquitos nf [
    let site one-of wolbachia-sites
    setxy [pxcor] of site [pycor] of site
    initialize-agent
    set wolbachia? true
    set eggs-remaining (eggs-remaining * Wolb-Affect-Fecundity-SCF)
    if (Wolb-Affect-Vectorial-Capacity > random-float 100) [set vector-capable? false]
    if (not wolbachia-hidden?) [set hidden? false]
  ]
end

to backfill-eggs
  let ovisites patches with [ovi-site?]
  let daily-eggs-per-k ((count mosquitos) * Eggs-Per-Brood / Gonotrophic-Cycle-Length) / sum [ovi-site-k] of ovisites
  ask ovisites [
    let n daily-eggs-per-k * ovi-site-k * (1 - sim-day / Backfill-Eggs-Day) / Ticks-Per-Day
    array:set eggs (array:item egg-index 0) ((array:item eggs (array:item egg-index 0)) + n) ;; add more eggs today
  ]
end

to hurricane-mockup
  ask n-of ((count mosquitos) * Hurricane-Kill-Ratio ) mosquitos [die]
  ask n-of ((count males) * Hurricane-Kill-Ratio ) males [die]    

  let ovisites patches with [ovi-site?]
  
  ask n-of ((count ovisites) * Hurricane-Kill-Ratio ) ovisites [
    set eggs array:from-list n-values (ticks-per-day * Egg-Hatch-Time) [0]
    set eggs-w array:from-list n-values (ticks-per-day * Egg-Hatch-Time) [0]
    set larvae array:from-list n-values (ticks-per-day * Eclose-Time) [0]
    set larvae-w array:from-list n-values (ticks-per-day * Eclose-Time) [0]
  ]    

end

;; vector control
to-report get-mosquito-spray-mortality-rate [sday]
  ifelse (sday <= 21)[
    report 100 * (1 - exp(ln(1 - mortality-rate-mosquito-base) / Ticks-Per-Day))
  ][ 
    report 100 * (1 - exp(ln(1 - mortality-rate-mosquito-base * exp(-(sday - 21) * mortality-rate-mosquito-decay)) / Ticks-Per-Day))
  ]
end

to-report get-larva-larvicide-survival-rate [lday]
  ifelse (lday <= 21)[
    report exp(ln(1 - mortality-rate-larvae-base) / Ticks-Per-Day)
  ][ 
    report exp(ln(1 - mortality-rate-larvae-base * exp(-(lday - 21) * mortality-rate-larvae-decay)) / Ticks-Per-Day)
  ]
end

; must be run by person, because it references "symptomatic"
to schedule-vector-control
  let skip-prob 10
  let random-prob 3
  ;let v-day 0
  let assign random 100
  
  if ((symptomatic = 1) or (assign <= random-prob)) [ 
    file-open (word Run-Name "/out-" run-number "-schedule.dat")

    if (symptomatic = 1)[
      set n-schedule n-schedule + 1
      ;output-print (word "schedule individual control!")
    
      ;; scheduled by infected case
      if (random 100 > skip-prob) [
        let in-compliant-house location-compliance-check bornx borny
        hatch-sprayed 1 [
          initialize-agent 
          ifelse (in-compliant-house = 1)[
            set xcor bornx
            set ycor borny
          ][
            set xcor (bornx + hwidth) ;; a point out of but close to house
            set ycor (borny + hwidth) ;; a point out of but close to house                       ]
          ]
          ifelse (control-init-day > sim-day) [
            ;set v-day (control-init-day + 1 + random 2) ;; maybe need to be random normal draw
            set spray-day (control-init-day + 5 + random 20) 
          ][
            ;set v-day (sim-day + 1 + random 2)
            set spray-day ((floor sim-day) + 5 + random 20)
          ]
          
          output-print (word "sprayed id " who " x-cor " xcor " y-cor " ycor)
          file-print (word (floor spray-day) " "
            (precision (xcor * Meters-Per-Patch) 3) " " (precision (ycor * Meters-Per-Patch) 3))
        ]      
      ]
    ]   

    ;; scheduled by randomly picking locations for prevention
    if (assign <= random-prob)[
      set n-schedule n-schedule + 1
      ;output-print (word "schedule random contorl!")
          
      hatch-sprayed 1 [
        initialize-agent
        ifelse (control-init-day > sim-day) [
          set spray-day (control-init-day + random 25) ;; maybe need to be random normal draw
        ][
          set spray-day ((floor sim-day) + random 25)
        ]
      
        set xcor (random-float (max-pxcor - min-pxcor) + min-pxcor )
        set ycor (random-float (max-pycor - min-pycor) + min-pycor ) 

        output-print (word "sprayed id " who " x-cor " xcor " y-cor " ycor)      
        file-print (word (floor spray-day) " "
          (precision (xcor * Meters-Per-Patch) 3) " " (precision (ycor * Meters-Per-Patch) 3))
      ]
    ]
    file-close
    output-print (word "number of sprayed " count sprayed)
  ]
end

;to load-vector-control
;  let fname (word Run-Name "/out-" run-number "-schedule.dat")
;  file-open fname
;  let nrec n-schedule
;  foreach n-values nrec [?] [
;    let vcday file-read
;    if (vcday = (floor sim-day)) [
;      ask onesprayed spray-sprout-number [
;        hatch-sprayed 1 [
;          initialize-agent
;          set spray-day vcday 
;          set bornx (file-read / Meters-Per-Patch)
;          set borny (file-read / Meters-Per-Patch)
;          output-print (word "sprayed number " who)
;        ]
;      ] 
;    ]
;  ]
;  file-close
;  output-print (word "number of sprayed " count sprayed)
;end

to vector-control-intervention
  ;ask sprayed [
    ask mosquitos in-radius (cover-radius / Meters-Per-Patch) [
      if (random 100 <= control-percent-mosquito) [
        let if-indoor location-indoor-check xcor ycor
        ifelse (if-indoor = 1) [ 
          let in-compliant-house location-compliance-check xcor ycor
          if (in-compliant-house = 1) [ 
            set spray-covered 1
            set own-sprayed-start-day (floor sim-day)
            ifelse ((SUSCEPTIBLE = color) or (SPRAYS = color)) [
              set color SPRAYS
            ][ ifelse ((EXPOSED = color) or (SPRAYE = color)) [
              set color SPRAYE 
            ][
              set color SPRAYI
            ]]
          ]
        ][
          set spray-covered 1
          set own-sprayed-start-day (floor sim-day)
          ifelse ((SUSCEPTIBLE = color) or (SPRAYS = color)) [
            set color SPRAYS
          ][ ifelse ((EXPOSED = color) or (SPRAYE = color)) [
            set color SPRAYE 
          ][
            set color SPRAYI
          ]]
        ]       
      ]     
    ]

    ask males in-radius (cover-radius / Meters-Per-Patch) [
      if (random 100 <= control-percent-mosquito) [
        let if-indoor location-indoor-check xcor ycor
        ifelse (if-indoor = 1) [ 
          let in-compliant-house location-compliance-check xcor ycor
          if (in-compliant-house = 1) [ 
            set spray-covered 1
            set own-sprayed-start-day (floor sim-day)
            set color SPRAY
          ]
        ][
          set spray-covered 1
          set own-sprayed-start-day (floor sim-day)
          set color SPRAY
        ]
      ]     
    ]
  ;]
end

; must be run by people, because it references symptomatic
to schedule-container-control
  let skip-prob 10
  let random-prob 3
  let assign random 100
  
  if ((symptomatic = 1) or (assign <= random-prob)) [ 
    file-open (word Run-Name "/out-" run-number "-schedule_reduction.dat")

    if (symptomatic = 1)[
      set n-reduction n-reduction + 1
    
      ;; scheduled by infected case
      if (random 100 > skip-prob) [
        let in-compliant-house location-compliance-check bornx borny
        hatch-reductions 1 [
          initialize-agent
          ifelse (in-compliant-house = 1)[
            set xcor bornx
            set ycor borny
          ][
            set xcor (bornx + hwidth) ;; a point out of but close to house
            set ycor (borny + hwidth) ;; a point out of but close to house
          ]
          ifelse (reduction-init-day > sim-day) [
            set reduction-day (reduction-init-day + 5 + random 20) ;; maybe need to be random normal draw
          ][
            set reduction-day ((floor sim-day) + 5 + random 20)
          ]
          
          output-print (word "reduction id " who " x-cor " xcor " y-cor " ycor)    
          file-print (word (floor reduction-day) " "
            (precision (xcor * Meters-Per-Patch) 3) " " (precision (ycor * Meters-Per-Patch) 3))
        ] 
      ]
    ]   

    ;; scheduled by randomly picking locations for prevention
    if (assign < random-prob)[
      set n-reduction n-reduction + 1
    
      hatch-reductions 1 [
        initialize-agent
        ifelse (reduction-init-day > sim-day) [
          set reduction-day (reduction-init-day + random 25) ;; maybe need to be random normal draw
        ][
          set reduction-day ((floor sim-day) + random 25)
        ]
      
        set xcor (random-float (max-pxcor - min-pxcor) + min-pxcor )
        set ycor (random-float (max-pycor - min-pycor) + min-pycor ) 

        output-print (word "reduction id " who " x-cor " xcor " y-cor " ycor)          
        file-print (word (floor reduction-day) " "
          (precision (xcor * Meters-Per-Patch) 3) " " (precision (ycor * Meters-Per-Patch) 3))
      ]
    ]
    file-close
    output-print (word "number of container reduced " count reductions)
  ]
end

to container-control-intervention
  ;ask reductions [
    ask patches in-radius (cover-radius / Meters-Per-Patch) [
      if (ovi-site?) [ 
        let if-indoor location-indoor-check pxcor pycor
        ifelse (if-indoor = 1) [ 
          let in-compliant-house location-compliance-check pxcor pycor
          if (in-compliant-house = 1) [
            output-print (word "number of eggs before: " sum array:to-list eggs)
             
            let i 0
            let j 0
            let idx 0
            while [i < Eclose-Time] [
              set j 0
              while [j < Ticks-Per-Day] [
                set idx i * ticks-per-day + j
                
                if (i < Egg-Hatch-Time) [
                  array:set eggs idx (array:item eggs idx) * (1 - control-percent-eggs / 100)
                  ;array:set eggs idx (array:item eggs idx) * exp(ln(1 - control-percent-eggs / 100) / Ticks-Per-Day)
                  ;array:set eggs-w idx (array:item eggs-w idx) * exp(ln(1 - control-percent-eggs / 100) / Ticks-Per-Day)
                ]

                array:set larvae idx (array:item larvae idx) * (1 - control-percent-larvae / 100)        
                ;array:set larvae idx (array:item larvae idx) * exp(ln(1 - control-percent-larvae / 100) / Ticks-Per-Day)
                ;array:set larvae-w idx (array:item larvae-w idx) * exp(ln(1 - control-percent-larvae / 100) / Ticks-Per-Day)
              
                set j j + 1
              ]
              set i i + 1
            ]
            
            output-print (word "number of eggs after: " sum array:to-list eggs)     
          ]
        ][
          output-print (word "number of eggs before: " sum array:to-list eggs) 
        
          let i 0
          let j 0
          let idx 0
          while [i < Eclose-Time] [
            set j 0
            while [j < Ticks-Per-Day] [
              set idx i * ticks-per-day + j
                
              if (i < Egg-Hatch-Time) [
                array:set eggs idx (array:item eggs idx) * (1 - control-percent-eggs / 100)
                ;array:set eggs idx (array:item eggs idx) * exp(ln(1 - control-percent-eggs / 100) / Ticks-Per-Day)
                ;array:set eggs-w idx (array:item eggs-w idx) * exp(ln(1 - control-percent-eggs / 100) / Ticks-Per-Day)
              ]

              array:set larvae idx (array:item larvae idx) * (1 - control-percent-larvae / 100)          
              ;array:set larvae idx (array:item larvae idx) * exp(ln(1 - control-percent-larvae / 100) / Ticks-Per-Day)
              ;array:set larvae-w idx (array:item larvae-w idx) * exp(ln(1 - control-percent-larvae / 100) / Ticks-Per-Day)
              
              set j j + 1
            ]
            set i i + 1
          ]
          
          output-print (word "number of eggs after: " sum array:to-list eggs)
        ]
        ;output-print (word "number of container reduced successfully!")       
      ]     
    ]
  ;]
end

; must be run by people, since it references "symptomatic"
to schedule-larvicide-control
  let skip-prob 10
  let random-prob 3
  let assign random 100
  
  if ((symptomatic = 1) or (assign <= random-prob)) [ 
    file-open (word Run-Name "/out-" run-number "-schedule_larvicide.dat")

    if (symptomatic = 1)[
      set n-larvicide n-larvicide + 1
    
      ;; scheduled by infected case
      if (random 100 > skip-prob) [
        let in-compliant-house location-compliance-check bornx borny
        hatch-larvicidesprayed 1 [
          initialize-agent
          ifelse (in-compliant-house = 1)[
            set xcor bornx
            set ycor borny
          ][
            set xcor (bornx + hwidth) ;; a point out of but close to house
            set ycor (borny + hwidth) ;; a point out of but close to house
          ]
          ifelse (larvicide-init-day > sim-day) [
            set larvicidespray-day (larvicide-init-day + 5 + random 20) ;; maybe need to be random normal draw
          ][
            set larvicidespray-day ((floor sim-day) + 5 + random 20)
          ]
          
          output-print (word "larvicide id " who " x-cor " xcor " y-cor " ycor)    
          file-print (word (floor larvicidespray-day) " "
            (precision (xcor * Meters-Per-Patch) 3) " " (precision (ycor * Meters-Per-Patch) 3))
        ] 
      ]
    ]   

    ;; scheduled by randomly picking locations for prevention
    if (assign < random-prob)[
      set n-larvicide n-larvicide + 1
    
      hatch-larvicidesprayed 1 [
        initialize-agent
        ifelse (reduction-init-day > sim-day) [
          set larvicidespray-day (larvicide-init-day + random 25) ;; maybe need to be random normal draw
        ][
          set larvicidespray-day ((floor sim-day) + random 25)
        ]
      
        set xcor (random-float (max-pxcor - min-pxcor) + min-pxcor )
        set ycor (random-float (max-pycor - min-pycor) + min-pycor ) 

        output-print (word "larvicide id " who " x-cor " xcor " y-cor " ycor)          
        file-print (word (floor larvicidespray-day) " "
          (precision (xcor * Meters-Per-Patch) 3) " " (precision (ycor * Meters-Per-Patch) 3))
      ]
    ]
    file-close
    output-print (word "number of larvicide sprayed " count larvicidesprayed)
  ]
end

to larvicide-control-intervention
  ;ask reductions [
    ask patches in-radius (cover-radius / Meters-Per-Patch) [
      if (ovi-site?) [ 
        let if-indoor location-indoor-check pxcor pycor
        ifelse (if-indoor = 1) [ 
          let in-compliant-house location-compliance-check pxcor pycor
          if (in-compliant-house = 1) [
            set larvicide-sprayed 1
            set own-larvicidesprayed-start-day (floor sim-day)     
          ]
        ][
          set larvicide-sprayed 1
          set own-larvicidesprayed-start-day (floor sim-day) 
        ]
        ;output-print (word "number of container reduced successfully!")       
      ]     
    ]
  ;]
end

to-report location-indoor-check [xcheck ycheck] 
  ;; check if the location of mosquitos or people indoor, if yes, report 1 

  report (OUTDOOR != ([bldg] of (patch xcheck ycheck)))
end

to-report location-compliance-check [xcheck ycheck] 
  
  report ((HOUSE1 = ([bldg] of (patch xcheck ycheck))) or (SCHOOL = ([bldg] of (patch xcheck ycheck))) or (WORK = ([bldg] of (patch xcheck ycheck))))

;  Village-Patches ;;use "house-compliance" to check if the location of mosquitos or people in compliant house, if yes, report 1 
;
;  report in-compliant-house
end

;; executed every tick (nominally, tick=hour)
to update-time
  ;; advance the global time
  tick
  set sim-day ticks / ticks-per-day
  set time-of-day ticks mod ticks-per-day
  set is-day? (time-of-day <= 16) ;; make sure this follows ticks-per-day !!!

  ;; allow delayed infection
  if (Propagate-Infection? and sim-day = Infection-Day-Start) [infect-agents]

  if (Wolbachia-Release-Start-Day >= 0 and sim-day >= Wolbachia-Release-Start-Day and sim-day <= Wolbachia-Release-End-Day and 0 = time-of-day) [
    ;; inject Wolbachia
    if (0 = (floor (sim-day - Wolbachia-Release-Start-Day)) mod Wolbachia-Release-Frequency) [
;      output-print (word "Day " sim-day ": new week... new injection")
      inject-wolbachia
    ]
  ]

  ;; seasonality
  ask patches with [bldg = OUTDOOR] [
    
    if (ovi-site?) [
      ;output-print (word "ovisite-k before adjusted by seasonality: " ovi-site-k)
      
      set ovi-site-k (ovi-site-k-max * sin (360 * (sim-day - peak-day + time-of-day / ticks-per-day) / 365 + 90) + ovi-site-k-max)

      ;output-print (word "ovisite-k after adjusted by seasonality: " ovi-site-k) 
    ]
 
  ]  
  
  ;; vector control
  ;if (sim-day = control-init-day) [set is-vector-control 1]
  
  ;if (is-vector-control = 1 and sim-day >= control-init-day and time-of-day = 12) [
  if (is-vector-control? and sim-day >= control-init-day and time-of-day = 12) [  
  ;  load-vector-control
    ask sprayed [ 
      if (spray-day = (floor sim-day)) [ 
        vector-control-intervention
      ]
    ]        
  ;  ask sprayed [ die ]
  ]
  
  ;if (sim-day = reduction-init-day) [set is-reduction-control 1]
  
  ;if (is-reduction-control = 1 and sim-day >= reduction-init-day and time-of-day = 12) [
  if (is-reduction-control? and sim-day >= reduction-init-day and time-of-day = 12) [  
    ask reductions [
      if (reduction-day = (floor sim-day)) [
        container-control-intervention 
      ]
    ]
  ]
  
  ;if (sim-day = larvicide-init-day) [set is-larvicide-control 1]
  
  ;if (is-larvicide-control = 1 and sim-day >= larvicide-init-day and time-of-day = 12) [
  if (is-larvicide-control? and sim-day >= larvicide-init-day and time-of-day = 12) [
    ask larvicidesprayed [
      if (larvicidespray-day = (floor sim-day)) [
        larvicide-control-intervention 
      ]
    ]
  ]

  if (0 = time-of-day and (round sim-day = Hurricane-Start-Day))[
    hurricane-mockup
  ]
  
  ;; allow leveling larvae
  if (Backfill-Eggs-Day > sim-day) [backfill-eggs]
  
  if (show-time?) [
    ask patch (max-pxcor - 5) (max-pycor - 7) [set plabel (round (100 * (sim-day))) / 100]
  ]
  
  ;; apply aging to specific mosquito characteristics
  ask mosquitos [
    set age (age + (1 / ticks-per-day))
    set body-clock (body-clock + 1 / ticks-per-day)
    set hunger-clock (hunger-clock + 1)
    
    ;; die...
    if (age >= age-max) [ die ] ;; old age
    let r Mosquito-Mortality-Rate-base + age * mosquito-mortality-rate-inc
    if (wolbachia?) [set r Mosquito-Mortality-Rate-base-w + age * mosquito-mortality-rate-inc-w]
    if (random 10000) < 100 * r [ die ] ;; daily hazzards, linear increase
    ;; vector control
    if (spray-covered = 1) [
      let rs get-mosquito-spray-mortality-rate (sim-day - own-sprayed-start-day)
      if (random 100) < rs [ die ]
    ]

; !!! REMEMBER: that Wolb-infections shorten mosquito lifespan by as much as 50%, due largely to feeding inhibition.  Older mosquitos are less able to take in blood meals

    ;; make infections progress
    if (Propagate-Infection?) [update-infection-state]

    ;; since all mosquitos are female, reproduce IFF the time and location is right
    if ((body-clock > 0) and (bites > 0) and (ovi-site?) and (mated?)) [
      reproduce
    ]
  ]

  ask males[
    set age (age + (1 / ticks-per-day))

    ;; die...
    if (age >= age-max) [ die ] ;; old age
    let r Mosquito-Mortality-Rate-base + age * mosquito-mortality-rate-inc
    if (wolbachia?) [set r Mosquito-Mortality-Rate-base-w + age * mosquito-mortality-rate-inc-w]
    if (random 10000) < 100 * r [ die ] ;; daily hazzards, linear increase
    ;; vector control
    if (spray-covered = 1) [
      let rs get-mosquito-spray-mortality-rate (sim-day - own-sprayed-start-day)
      if (random 100) < rs [ die ]
    ]
  ]
  
  ;; check for eclosion / hatching eggs  
  update-aquatic-indeces ;; do this first
  ask patches with [ovi-site?][
    update-aquatic-states
  ]
  ;test
  ;output-print (word "n-egg-w %: " (100 * (sum [n-eggs-w] of (patches with [ovi-site?])) / (sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]))))
  
  if (Wolbachia-Release-Start-Day <= sim-day and sim-day <= Wolbachia-Release-End-Day) [ 
    file-open (word Run-Name "/out-" run-number "-wolbachia_egg_percentage.dat")   
    file-print (word (floor sim-day) " " time-of-day " " (100 * (sum [n-eggs-w] of (patches with [ovi-site?])) / (sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]))))  
    file-close  
  ]
  
  if (Propagate-Infection?) [
    ask people [
      ;; make infections progress
      update-infection-state
    ]
  ]
end

to update-aquatic-indeces
  array:set egg-index 0 (array:item egg-index 0) - 1
  if (array:item egg-index 0) < 0 [array:set egg-index 0 (array:item egg-index 1)]
  array:set larva-index 0 (array:item larva-index 0) - 1
  if (array:item larva-index 0) < 0 [array:set larva-index 0 (array:item larva-index 1)]
end

to update-aquatic-states
  ;; apply daily mortality
  ;; for eggs/larvae, these are just count, so it's a straight survival multiplier

  let s-e  exp(ln(1 - Egg-Mortality-Rate / 100) / Ticks-Per-Day)
  let s-ew exp(ln(1 - Egg-Mortality-Rate * Wolb-Affect-Egg-SCF / 100) / Ticks-Per-Day)
  let s-l  exp(ln(1 - get-larva-mortality-rate / 100) / Ticks-Per-Day)
  let s-lw exp(ln(1 - get-larva-mortality-rate * Wolb-Affect-Larva-SCF / 100) / Ticks-Per-Day)
  
  ;; vectorl control
  if (is-larvicide-control? and larvicide-sprayed = 1) [
    set s-l get-larva-larvicide-survival-rate (sim-day - own-larvicidesprayed-start-day)
    set s-lw get-larva-larvicide-survival-rate (sim-day - own-larvicidesprayed-start-day) * Wolb-Affect-Larva-SCF   
  ]  

;  if (pverbose?) [output-show (word "ovi-dens = " ovi-dens ", ovi-site-k = " ovi-site-k ", n-larvae = " n-larvae ", n-larvae-w = " n-larvae-w ", s-l = " s-l ", s-lw = " s-lw)]
;  if (pverbose?) [output-show (word "s-l = " precision s-l 4 ", s-lw = " precision s-lw 4)]

  let i 0
  let j 0
  let idx 0
  while [i < Eclose-Time] [
    set j 0
    while [j < Ticks-Per-Day] [
      set idx i * ticks-per-day + j

      ifelse use-random-binomial?[
        if (i < Egg-Hatch-Time) [
          ;test
          ;output-print (word "s-e: " s-e " s-ew:" s-ew)
          if (0 < array:item eggs idx)   [array:set eggs idx random-binomial (array:item eggs idx) s-e]
          if (0 < array:item eggs-w idx) [array:set eggs-w idx random-binomial (array:item eggs-w idx) s-ew]
        ]
        ;test
        ;output-print (word "s-l: " s-l " s-lw:" s-lw)
        if (0 < array:item larvae idx)   [array:set larvae idx random-binomial (array:item larvae idx) s-l]
        if (0 < array:item larvae-w idx) [array:set larvae-w idx random-binomial (array:item larvae-w idx) s-lw]
      ][
        if (i < Egg-Hatch-Time) [
          array:set eggs idx (array:item eggs idx) * s-e
          array:set eggs-w idx (array:item eggs-w idx) * s-ew
        ]
        
        array:set larvae idx (array:item larvae idx) * s-l 
        array:set larvae-w idx (array:item larvae-w idx) * s-lw
      ]
            
      set j j + 1
    ]
    set i i + 1
  ]

  ;; first the Wild Types  
  let ready-eggs array:item eggs (array:item egg-index 0)
  array:set eggs (array:item egg-index 0) 0
  let ready-larvae array:item larvae (array:item larva-index 0)
  array:set larvae (array:item larva-index 0) ready-eggs

;  if (pverbose?) [output-show (word "ready-eggs = " precision ready-eggs 2 ", ready-larvae = " precision ready-larvae 2)]
  
  let m (ready-larvae * Male-Mosquito-Ratio / 100)
  sprout-males m [ initialize-agent ]
  if ((m mod 1) > random-float 1) [sprout-males 1 [ initialize-agent ]]
  sprout-mosquitos (ready-larvae - m) [initialize-agent]
  if (((ready-larvae - m) mod 1) > random-float 1) [sprout-mosquitos 1 [ initialize-agent ]]

  ;; now the Wolbachia
  set ready-eggs array:item eggs-w (array:item egg-index 0)
  array:set eggs-w (array:item egg-index 0) 0
  set ready-larvae array:item larvae-w (array:item larva-index 0)
  array:set larvae-w (array:item larva-index 0) ready-eggs

  set m (ready-larvae * Male-Mosquito-Ratio / 100)
  sprout-males m [
    initialize-agent 
    set wolbachia? true
  ]
  if ((m mod 1) > random-float 1) [sprout-males 1 [ initialize-agent set wolbachia? true]]
  sprout-mosquitos (ready-larvae - m) [
    initialize-agent
    set wolbachia? true
    set eggs-remaining (eggs-remaining * Wolb-Affect-Fecundity-SCF)
    if (Wolb-Affect-Vectorial-Capacity > random-float 100) [set vector-capable? false]
  ]
  if (((ready-larvae - m) mod 1) > random-float 1) [
    sprout-mosquitos 1 [
      initialize-agent
      set wolbachia? true
      set eggs-remaining (eggs-remaining * Wolb-Affect-Fecundity-SCF)
      if (Wolb-Affect-Vectorial-Capacity > random-float 100) [set vector-capable? false]
    ]
  ]

end

;; this function is run by an individual agent
to update-infection-state
;print-agent self "updating infection state"
  ifelse (0 < itime) and     ;; there's an active infection
    (itime <= ticks) [   ;; it's time to transition
;; EXPOSED case
      ifelse (EXPOSED = color) [
        set color INFECTIOUS
        ifelse (is-person? self) [
          set itime ticks + get-infectious-time
        ][
          set itime 0 ;; mosquitos never "recover"
        ]
;print-agent self "now INFECTIOUS"
      ][

;; INFECTIOUS case
      ifelse (INFECTIOUS = color) [
        ;; only humans get here, because no timer is set for INFECTIOUS mosquitos
        ifelse (0 != first-strain) [
          set itime 0
          set color IMMUNED
;print-agent self "now IMMUNED"
        ]
        [
          set itime ticks + (tempimmune-time * ticks-per-day)
          set color TEMPIMMUNE
;print-agent self "now TEMPIMMUNE"
        ]
      ][ 

;; TEMPIMMUNE case
      ifelse (TEMPIMMUNE = color) [
        set itime 0
        set color RECOVERED
        set first-strain strain
        set strain 0
;print-agent self "now RECOVERED"
      ][;; vector control
        ifelse (SPRAYE = color) [
          set color SPRAYI
          set itime 0
      ][

;; default case (did not match any of the above named states)
      print-agent self "something is WRONG!!!"
      ]]]]
    ][
;print-agent self "no change"
    ]
end
  
;; called per-mosquito
to reproduce
  let me self ;; for some reason, using "myself" below wasn't working... so held the mother's ID here
  
  if not sterile? [ ; if females are sterile, they still *act* like they're laying eggs... they just don't result in larvae
;    let neggs (eggs-remaining - random-exponential Min-Eggs-Per-Oviposit)
;    set neggs max (list neggs Min-Eggs-Per-Oviposit)
    let neggs Min-Eggs-Per-Oviposit ;; ensure multi-ovisite laying
    if (wolbachia?) [set neggs (neggs * Wolb-Affect-Fecundity-SCF)]
    let neggs-w 0
    if (eggs-remaining - neggs < Min-Eggs-Per-Oviposit) [set neggs eggs-remaining] ;; don't leave a worthless, tiny batch
    set eggs-remaining (eggs-remaining - neggs) ;; subtract them from the total

    if (pverbose?) [output-show (word "patch " pxcor " " pycor ": laying " precision neggs 1 " eggs, " precision eggs-remaining 1 " remaining")]
  
    if (wolbachia?) [
      set neggs-w (random-binomial neggs ((100 - Wolb-Affect-Leakage-Rate) / 100)) ;; whatever hasn't "leaked", is actually a wolbachia egg...
      set neggs (neggs - neggs-w) ;; whatever did leak becomes a "plain" egg
    ]

    if (0 < neggs-w) [array:set eggs-w (array:item egg-index 0) ((array:item eggs-w (array:item egg-index 0)) + neggs-w)]
    if (0 < neggs)   [array:set eggs (array:item egg-index 0) ((array:item eggs (array:item egg-index 0)) + neggs)]

;    if (pverbose?) [output-show eggs]
  
    ifelse (eggs-remaining < 1)[
      ;; reset the female's body clock (can lay every X days...)
      set body-clock (- Gonotrophic-Cycle-Length) +  (random-float 2) - 1
      set eggs-remaining Eggs-Per-Brood * (1 - (((age - body-clock) / Max-Mosquito-Age) * (Age-Dep-Fecundity-Reduction / 100))) ;; look ahead to laying time
      if (wolbachia?) [set eggs-remaining (eggs-remaining * Wolb-Affect-Fecundity-SCF)]
      set bites 0 ;; used it up
      set hungry? true
    ][
      set go-straight Straight-Travel-After-Ovi
    ]
  ]
end

to draw-patches
  ifelse (Patch-Color-Shows = "buildings")[
    draw-buildings
  ][
  ifelse (Patch-Color-Shows = "ovi-dens")[
    draw-ovi-dens
  ][
  ifelse (Patch-Color-Shows = "wovi-dens")[
    draw-wovi-dens
  ][
  ifelse (Patch-Color-Shows = "host-dens")[
    draw-host-dens
  ][
  ifelse (Patch-Color-Shows = "mosq-dens")[
    draw-mosq-dens
  ][
  ifelse (Patch-Color-Shows = "shade")[
    draw-shade
  ][
  ifelse ("wolb" = substring Patch-Color-Shows 0 4)[ ; all in the same variable
    draw-wolbachia
  ][
;; NONE requested
  ]]]]]]]

  draw-palette
end

to draw-buildings
  ask patches [set pcolor bldg]
end

to draw-ovi-dens
  ask patches [
    let d (ovi-dens / Ovi-Dens-Max * 10)
    if (d > (length palette - 1)) [set d length palette - 1]
    set pcolor item d palette
  ]
end

to draw-wovi-dens
  ask patches [
    let d (wovi-dens / Wovi-Dens-Max * 10)
    if (d > (length palette - 1)) [set d length palette - 1]
    set pcolor item d palette
  ]
end

to draw-host-dens
  ask patches [
    let d (host-dens / Host-Dens-Max * 10)
    if (d > (length palette - 1)) [set d length palette - 1]
    set pcolor item d palette
  ]
end

to draw-mosq-dens
  ask patches [
    let d (mosq-dens / Mosq-Dens-Max * 10)
    if (d > (length palette - 1)) [set d length palette - 1]
    set pcolor item d palette
  ]
end

to draw-wolbachia
  ask patches [
    let d wolb-dens * (length palette - 1) ;; wolb-dens is from 0 to 1
    if (d > (length palette - 1)) [set d length palette - 1]
    set pcolor item d palette
  ]
end

to draw-shade
  ask patches [
    let d (shade * 10)
    if (d > (length palette - 1)) [set d length palette - 1]
    set pcolor item d palette
  ]
end

to draw-palette
  ifelse (Draw-Palette?)[
    if (not (patch-color-shows = "buildings")) [
      foreach n-values 10 [?] [
        ask patch max-pxcor (max-pycor - 10 + ?) [ set pcolor item ? palette ]
      ]
    ]
    ask patch (max-pxcor - 3) (max-pycor - 3) [set plabel patch-color-shows]
  ][
    ask patch (max-pxcor - 3) (max-pycor - 3) [set plabel ""] ;; wipe the label
  ]
end

to my-update-plots
  let m (count mosquitos)
  let p (count people)
  
  set-current-plot "Totals"
  set-current-plot-pen "mosquitos"
  plot m
;  set-current-plot-pen "people"
;  plot p
  set-current-plot-pen "males"
  plot (count males)
  set-current-plot-pen "larvae/20"
  plot sum [n-larvae + n-larvae-w] of (patches with [ovi-site?]) / 20
  set-current-plot-pen "eggs/20"
  plot sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]) / 20

;  if (0 = time-of-day) [
    update-aquatic-age-plots
;  ]

  if (any? mosquitos) [
    set-current-plot "Mosquito Ages"
    set-current-plot-pen "mosquitos"
    histogram [age] of mosquitos
    if (ticks = 0) [
      set-current-plot-pen "initial"
      histogram [age] of mosquitos
    ]      
;    set-current-plot-pen "males"
;    histogram [age] of males
    
    set-current-plot "Mosquito Travel"
    set-current-plot-pen "mosquitos"
    histogram [Meters-Per-Patch * sqrt((xcor - bornx) ^ 2 + (ycor - borny) ^ 2)] of mosquitos

    update-travel-profile
    set-current-plot "Travel Profile"
    plot-pen-reset
    let i 0
    while [i < array:length travel-profile] [
      plotxy i array:item travel-profile i
      set i i + 1
    ]

    set-current-plot "Bites"
    set-current-plot-pen "default"
    histogram [bites] of mosquitos
    
    set-current-plot "SEI - Mosquitos"
;    set-current-plot-pen "s"
;    set m 10
;    plot (count mosquitos with [color = SUSCEPTIBLE]) / m
;    set m 1
    set-current-plot-pen "e"
    plot (count mosquitos with [color = EXPOSED]) / m
    set-current-plot-pen "i"
    plot (count mosquitos with [color = INFECTIOUS]) / m

    set-current-plot "Percentages"
    set-current-plot-pen "Mated"
    plot (count mosquitos with [mated?] / count mosquitos) * 100
;    set-current-plot-pen "Wolb-Fem"
;    plot (count mosquitos with [Wolbachia?] / count mosquitos) * 100
;    set-current-plot-pen "Wolb-Mal"
;    plot (count males with [Wolbachia?] / count males) * 100
;    set-current-plot-pen "Wolb-Larvae"
;    plot (count mosquitos with [mated?] / count mosquitos)    

    set-current-plot "Age of Unmated Females"
    ifelse (count mosquitos with [not mated?]) > 0 [
      set-current-plot-pen "Mean"
      plot mean [age] of mosquitos with [not mated?]
      set-current-plot-pen "Median"
      plot median [age] of mosquitos with [not mated?]
      set-current-plot-pen "Max"
      plot max [age] of mosquitos with [not mated?]
    ][ 
      set-current-plot-pen "Mean"
      plot 0
      set-current-plot-pen "Median"
      plot 0
      set-current-plot-pen "Max"
      plot 0
    ]

    set-current-plot "Vector Capable"
    plot 100 * (count mosquitos with [vector-capable?]) / (count mosquitos)
  ]

  if (any? people) [
    set-current-plot "SEITRM - People"
;    set-current-plot-pen "s"
;    set p 10
;    plot (count people with [color = SUSCEPTIBLE]) / p
;    set p 1
    set-current-plot-pen "e"
    plot (count people with [color = EXPOSED]) / p
    set-current-plot-pen "i"
    plot (count people with [color = INFECTIOUS]) / p
;    set-current-plot-pen "t"
;    plot (count people with [color = TEMPIMMUNE]) / p
;    set-current-plot-pen "r"
;    plot (count people with [color = RECOVERED]) / p
;    set-current-plot-pen "m"
;    plot (count people with [color = IMMUNED]) / p
  ]

  let ovisites village-patches with [ovi-site?]
  let fm mosquitos-on village-patches
  let mm males-on village-patches
  set-current-plot "Wolbachia Penetration"
  set-current-plot-pen "Adult-M"
  ifelse (count mm) > 0 [
    plot 100 * (count mm with [wolbachia?]) / (count mm)
  ][
    plot 0
  ]  
  set-current-plot-pen "Adult-F"
  ifelse (count fm) > 0 [
    plot 100 * (count fm with [wolbachia?]) / (count fm)
  ][
    plot 0
  ]  
  set-current-plot-pen "Eggs"
  ifelse (sum [n-eggs + n-eggs-w] of ovisites) > 0 [
    plot 100 * (sum [n-eggs-w] of ovisites) / (sum [n-eggs + n-eggs-w] of ovisites)
  ][
    plot 0
  ]
  set-current-plot-pen "Larvae"
  ifelse (sum [n-larvae + n-larvae-w] of ovisites) > 0 [
  plot 100 * (sum [n-larvae-w] of ovisites) / (sum [n-larvae + n-larvae-w] of ovisites)
  ][
    plot 0
  ]
  set-current-plot-pen "Sterile"
  ifelse (count fm) > 0 [
    plot 100 * (count fm with [sterile?]) / (count fm)
  ][
    plot 0
  ]
end

to update-travel-profile
  let i 0
  while [i < array:length travel-profile] [
    let mos mosquitos with [(not migrant?) and (age >= i) and (age < i + 1)]
    ifelse any? mos [
      array:set travel-profile i (Meters-Per-Patch * mean [sqrt((xcor - bornx) ^ 2 + (ycor - borny) ^ 2)] of mos)
    ][
      array:set travel-profile i 0
    ]
    set i i + 1
  ]
end

to update-aquatic-age-plots
  let ovisites patches with [ovi-site?]
  ask ovisites [
    set egg-ages array:from-list n-values Egg-Hatch-Time [0]
    set larva-ages array:from-list n-values Eclose-Time [0]

    let i 0
    while [i < Eclose-Time] [
      let es 0
      let ls 0
      let j 0
      let idx 0
      while [j < Ticks-Per-Day] [
        set idx i * ticks-per-day + j
        let eidx ((idx + array:item egg-index 0) mod ((array:item egg-index 1) + 1))
        let lidx ((idx + array:item larva-index 0) mod ((array:item larva-index 1) + 1))
        if (i < Egg-Hatch-Time) [
          set es es + (array:item eggs eidx)
          set es es + (array:item eggs-w eidx)
        ]
        set ls ls + (array:item larvae lidx)
        set ls ls + (array:item larvae-w lidx)
        set j j + 1
      ]
      
      if (i < Egg-Hatch-Time) [array:set egg-ages i es]
      array:set larva-ages i ls            
      set i i + 1
    ]
  ]
  
  set-current-plot "Egg Ages"
  set-current-plot-pen "age"
  plot-pen-reset
  let i 0
  while [i < Egg-Hatch-Time][
    plotxy i sum [array:item egg-ages i] of ovisites
    set i i + 1
  ]
  if (ticks = 0) [
    set-current-plot-pen "initial"
    set i 0
    while [i < Egg-Hatch-Time][
      plotxy i sum [array:item egg-ages i] of ovisites
      set i i + 1
    ]
  ]

  set-current-plot "Larva Ages"
  set-current-plot-pen "age"
  plot-pen-reset
  set i 0
  while [i < Eclose-Time][
    let s sum [array:item larva-ages i] of ovisites
    if (s > 1) [plotxy i log s 10]
;    plotxy i s
    set i i + 1
  ]
  if (ticks = 0) [
    set-current-plot-pen "initial"
    set i 0
    while [i < Eclose-Time][
      let s sum [array:item larva-ages i] of ovisites
      if (s > 1) [plotxy i log s 10]
;      plotxy i s
      set i i + 1
    ]
  ]
end

to read-person-file
  let dummy 0

  let fname (word Input-File-Root "-people.dat")
  output-print (word "opening '" fname "'")
  file-open fname
  set Initial-People file-read
  output-print (word "reading " Initial-People " people")
  foreach n-values Initial-People [?] [
    create-people 1 [
      initialize-agent
      set dummy file-read ; but don't store this...
      set age file-read
      set bornx (file-read / Meters-Per-Patch)
      set borny (file-read / Meters-Per-Patch)

;; catch rounding errors...
      if (bornx > max-pxcor) [set bornx max-pxcor]
      if (bornx < min-pxcor) [set bornx min-pxcor]
      if (borny > max-pycor) [set borny max-pycor]
      if (borny < min-pycor) [set borny min-pycor]

      set wrkx (file-read / Meters-Per-Patch)
      set wrky (file-read / Meters-Per-Patch)

;; catch rounding errors...
      if (wrkx > max-pxcor) [set wrkx max-pxcor]
      if (wrkx < min-pxcor) [set wrkx min-pxcor]
      if (wrky > max-pycor) [set wrky max-pycor]
      if (wrky < min-pycor) [set wrky min-pycor]

;; start @ home
      set xcor bornx
      set ycor borny

      set  dummy file-read ; that's the commute dist
      
;      output-print (word "new person lives at (" xcor ", " ycor ")")
      set color SUSCEPTIBLE

    ]
  ]
  file-close
end

to read-command-file [fname]
  output-print (word "opening command input file: " fname)
  file-open fname
  while [not file-at-end?][
    let cmd file-read-line
    output-print cmd
    run cmd
  ]
  file-close
end

to read-checkpoint
  let fname (word "checkpoint-" run-name ".csv")
  if not file-exists? fname [set fname "checkpoint.csv"]
  if not file-exists? fname [
    output-print (word "cannot open " fname ": no checkpoint file found.")
    all-stop
  ]
  reset-timer
  output-print (word "loading checkpoint file: " fname)
  import-world fname
  output-print (word "finished loading checkpoint: " timer " sec")
end

to read-roads
  ask patches [
    set is-road? false
;    set pcolor black
  ]
  file-open (word "inputs/" roads-file)
  while [not file-at-end?] [
    ask patch file-read file-read [
      set is-road? true
      set pcolor white
    ]
  ]
  file-close
end

to read-location-file
  let id 0
  let typ 0
  let dum 0
  let nloc 0
  set nhouses 0
  
  let fname (word Input-File-Root "-locations.dat")
  output-print (word "opening '" fname "'")
  file-open fname
  set hwidth file-read / Meters-Per-Patch
  set xsep file-read / Meters-Per-Patch
  set ysep file-read / Meters-Per-Patch
  set Ovisite-Diffusion (hwidth * 5)
  output-print (word "hwidth, xsep, ysep, Meters-Per-Patch = " hwidth ", " xsep ", " ysep ", " Meters-Per-Patch)

  ; "speed" here is in meters
  ; "step" is measured in patches
  set Mosquito-Migration-Step (Mosquito-Migration-Speed / Meters-Per-Patch)
  set Person-Migration-Step (Person-Migration-Speed / Meters-Per-Patch)
  
  set bldg-xmin file-read / Meters-Per-Patch
  set bldg-xmax file-read / Meters-Per-Patch
  set bldg-ymin file-read / Meters-Per-Patch
  set bldg-ymax file-read / Meters-Per-Patch
  
  resize-world (bldg-xmin - (Outdoor-Ovisite-Border-Width + Void-Border-Width) / Meters-Per-Patch) (bldg-xmax + (Outdoor-Ovisite-Border-Width + Void-Border-Width) / Meters-Per-Patch) (bldg-ymin - (Outdoor-Ovisite-Border-Width + Void-Border-Width) / Meters-Per-Patch) (bldg-ymax + (Outdoor-Ovisite-Border-Width + Void-Border-Width) / Meters-Per-Patch)
  output-print (word "world is now (" min-pxcor ", " max-pxcor ", " min-pycor ", " max-pycor ")")
  output-print (word "village is (" bldg-xmin ", " bldg-xmax ", " bldg-ymin ", " bldg-ymax ")")
  
  set Village-Patches patches with [pxcor >= bldg-xmin and pxcor <= bldg-xmax and pycor >= bldg-ymin and pycor <= bldg-ymax]

  clear-patches
  ask patches [
    set ovi-site? false
    set ovi-site-k 0
    set pverbose? false
    set is-road? false
    set front-door-id 0
    set eggs array:from-list n-values (ticks-per-day * Egg-Hatch-Time) [0]
    set eggs-w array:from-list n-values (ticks-per-day * Egg-Hatch-Time) [0]
    set larvae array:from-list n-values (ticks-per-day * Eclose-Time) [0]
    set larvae-w array:from-list n-values (ticks-per-day * Eclose-Time) [0]
    set shade .2
  ]

  if (Void-Border-Width > 0)[
    foreach n-values (Void-Border-Width / Meters-Per-Patch) [?] [
      ask patches with [pxcor = (max-pxcor - (?))][set bldg BORDER set shade 0]
      ask patches with [pxcor = (min-pxcor + (?))][set bldg BORDER set shade 0]
      ask patches with [pycor = (max-pycor - (?))][set bldg BORDER set shade 0]
      ask patches with [pycor = (min-pycor + (?))][set bldg BORDER set shade 0]
    ]
  ]
  
  set nloc file-read
  output-print (word "reading " nloc " locations")
  let nother 0
  foreach n-values nloc [?] [
    set id file-read
    set typ file-read
    let x (file-read / Meters-Per-Patch)
    let y (file-read / Meters-Per-Patch)
;    if (id = 0) [output-print (word "id " id ", type " typ ", x " x ", y " y)]
    let thiscol 0
    ifelse (typ = 1)[
      set thiscol HOUSE
      set nhouses (nhouses + 1)
    ][ ifelse (typ = 2)[
      set thiscol WORK
      set nother (nother + 1)
    ][ ifelse (typ = 3) [
      set thiscol SCHOOL
      set nother (nother + 1)
    ][ ifelse (typ = 4) [;; vector control
      set thiscol HOUSE1
      set nhouses (nhouses + 1)
    ][
      output-print "bad typ value"
    ]]]]
    foreach n-values hwidth [?] [
      let xprime ? - (hwidth / 2)
      foreach n-values hwidth [?] [
        let yprime ? - (hwidth / 2)
        ask patch (x + xprime) (y + yprime) [
          set shade 1.
          set bldg thiscol
          if (random (100 * hwidth * hwidth) < Breteau-Index) [
            init-ovisite true
          ]
        ]
      ]
    ]

    if (typ = 1) [
      ask patch (x - hwidth / 2 - 1) y [
        set front-door-id nhouses
      ]
    ]
    
    set dum file-read ; capacity
    set dum file-read ; occupation
;    output-print (word "done with loc " id)
  ]
  file-close

  set max-bldgs (1 + (bldg-xmax - bldg-xmin) / xsep) * (1 + (bldg-ymax - bldg-ymin) / ysep)
  set nproperties ((bldg-xmax - bldg-xmin) / xsep) * ((bldg-ymax - bldg-ymin) / ysep)
  set bldg-density (nhouses + nother) / max-bldgs
  output-print (word "max-bldgs = " max-bldgs ", nhouses = " nhouses ", nother = " nother ", bldg-density = " bldg-density ", nproperties = " nproperties)
end

to init-ovisite [indoor?]
  set ovi-site? true
  set ovi-dens 1 ; must seed this... (100 larvae / 100 L) some higher, some lower
  set ovi-site-k sample-from-container-cdf
  
  if not indoor? [
    set ovi-site-k-max ovi-site-k * (random-float Outdoor-Ovisite-K-Ratio-Max) ;; seasonality
  ]
  
  if (Heterogenous-K?) [
    let xrat pxcor / max-pxcor
    let yrat pycor / max-pycor
    set ovi-site-k ovi-site-k * (1 + (xrat + yrat) / 2.5)
  ]
end

to-report sample-from-brood-cdf
  let f random-float 1
  foreach brood-cdf [
    if (f < last ?)[
     report (first ?)
    ]
  ]
end

to-report sample-from-container-cdf
  let f random-float 1
  foreach container-size-cdf [
    if (f < last ?)[
      report (first ?)
    ]
  ]
end

;; get a random number, sampled from a gaussian
;; takes the centroid and width as input
to-report sample-from-gaussian [centroid width]
  let r 0
  let r1 0
  let r2 0
  while [r = 0 or r > 1] [
    set r1 (2 * (random-float 1)) - 1
    set r2 (2 * (random-float 1)) - 1
    set r r1 * r1 + r2 * r2
  ]
  let val (r1 * sqrt(-2 * ln(r) / r))

  report (val * width + centroid)
end

;; note that although we use "maximum" as a parameter, the Rayleigh distribution has a long exponential tail
;; so this is only a figure of merit, assuming a nominal width of 3 for the un-qualified Rayleigh distribution  
to-report sample-from-rayleigh [minimum maximum]
  let r (random-float 1)
  let width (maximum - minimum) / 3 ;; scaled by the nominal width of 3
  report (minimum + sqrt(-2 * ln(1 - r)) * width)
end

;; called when someone gets infected
to log-infection
  let atype 0 ;; mosquito
  if (is-person? self) [set atype 1]
  
  if (log-mosquito-infections? or 1 = atype)[
    file-open (word Run-Name "/out-" run-number ".ilog")
    
    ;; NOTE:  If you change this, be sure to update the header file in setup-files
    ;;file-print (word igen " " atype " " who " " 
    ;;  (precision (infectedx * Meters-Per-Patch) 3) " " (precision (infectedy * Meters-Per-Patch) 3) " " 
    ;;  (precision (bornx * Meters-Per-Patch) 3) " " (precision (borny * Meters-Per-Patch) 3) " "
    ;;  (precision infector 3) " " (precision (infectedx-1 * Meters-Per-Patch) 3) " " (precision (infectedy-1 * Meters-Per-Patch) 3) " "
    ;;  infector-1 " " (precision sim-day 3))
    ;; vector control
    file-write (word igen " " atype " " who " " 
      (precision (infectedx * Meters-Per-Patch) 3) " " (precision (infectedy * Meters-Per-Patch) 3) " " 
      (precision (bornx * Meters-Per-Patch) 3) " " (precision (borny * Meters-Per-Patch) 3) " "
      (precision infector 3) " " (precision (infectedx-1 * Meters-Per-Patch) 3) " " (precision (infectedy-1 * Meters-Per-Patch) 3) " "
      infector-1 " " (precision sim-day 3) " " (precision itime 3) " ")

    ifelse (is-person? self) 
      [file-print symptomatic] ;; only humans have symptomaticity
      [file-print 0] ;; vectors don't show symptomaticity

    file-close
  ]
end

to go-headless [rnum]
  output-print (word "setup run " rnum)
  setup

;  import-world "init-30d.csv"
;  set Infection-Day-Start 30

  no-display                                                                                                                     
  set view-on? false                                                                                                             
  set plots-on? false                                                                                                            
  random-seed rnum
  set run-number rnum
  let steps Max-Days * ticks-per-day - ticks
  output-print (word "go " steps "...")
  repeat steps [ go ]

  output-print (word sim-timer " sec")
end
@#$#@#$#@
GRAPHICS-WINDOW
35
898
525
1409
-1
-1
5.0
1
20
1
1
1
0
1
1
1
-45
50
-45
50
1
1
1
hours
30.0

BUTTON
956
48
1030
81
NIL
Setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
1043
48
1106
81
NIL
Go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

MONITOR
1794
47
1870
92
people
count people
17
1
11

MONITOR
1794
96
1871
141
mosquitos
count mosquitos\n
17
1
11

PLOT
1412
48
1782
214
Totals
Day
#
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"mosquitos" 0.04166667 0 -2674135 true "" ""
"Males" 0.04166667 0 -7500403 true "" ""
"larvae/20" 0.04166667 0 -10899396 true "" ""
"eggs/20" 0.04166667 0 -1184463 true "" ""

SLIDER
17
99
299
132
Max-Mosquito-Age
Max-Mosquito-Age
7
100
21
1
1
days
HORIZONTAL

SLIDER
954
155
1237
188
Max-Days
Max-Days
0
400
259
1
1
NIL
HORIZONTAL

SLIDER
15
49
298
82
Initial-Mosquito-to-Human-Ratio
Initial-Mosquito-to-Human-Ratio
.5
10
6
.5
1
:1
HORIZONTAL

TEXTBOX
1416
14
1566
38
Output Metrics
20
0.0
1

TEXTBOX
952
15
1102
39
Run Control
20
0.0
1

TEXTBOX
21
12
205
42
Inputs: Mosquito
20
15.0
1

TEXTBOX
643
252
793
276
Inputs: People
20
105.0
1

PLOT
1413
219
1716
383
Mosquito Ages
Day
#
0.0
25.0
0.0
10.0
true
false
"" ""
PENS
"mosquitos" 1.0 1 -2674135 true "" ""
"males" 1.0 0 -7500403 true "" ""
"initial" 1.0 0 -13345367 true "" ""

SLIDER
16
151
304
184
Mosquito-Mortality-Rate-Min
Mosquito-Mortality-Rate-Min
0
20
11
.5
1
%/day
HORIZONTAL

SLIDER
17
258
300
291
Bite-Probability
Bite-Probability
0
100
90
5
1
%
HORIZONTAL

PLOT
1798
585
2108
756
Mosquito Travel
Meters
#
0.0
500.0
0.0
10.0
true
false
"" ""
PENS
"mosquitos" 20.0 1 -2674135 true "" ""
"people" 1.0 1 -13345367 true "" ""

MONITOR
2115
585
2234
630
Mean Travel (m)
Meters-Per-Patch * mean [sqrt((xcor - bornx) ^ 2 + (ycor - borny) ^ 2)] of mosquitos
2
1
11

OUTPUT
23
1810
1766
2117
20

SLIDER
1417
1435
1709
1468
Initial-Mosquito-Infection-Rate
Initial-Mosquito-Infection-Rate
0
10
0.5
0.1
1
%
HORIZONTAL

MONITOR
1630
728
1680
773
r
(count people with [ color = RECOVERED])
17
1
11

MONITOR
1724
437
1781
482
e
(count mosquitos with [ color = EXPOSED])
17
1
11

MONITOR
1524
728
1574
773
i
(count people with [ color = INFECTIOUS])
17
1
11

SLIDER
19
304
301
337
Bite-Radius
Bite-Radius
0
10
5
.1
1
m
HORIZONTAL

SLIDER
19
354
303
387
Time-Between-Bites
Time-Between-Bites
1
12
8
1
1
hours
HORIZONTAL

BUTTON
1121
48
1230
81
Go (one step)
output-print \"Begin: <Go one step>\"\nGo\noutput-print \"End: <Go one step>\"
NIL
1
T
OBSERVER
NIL
1
NIL
NIL
1

MONITOR
1170
95
1250
140
NIL
sim-day
2
1
11

PLOT
1414
387
1716
565
SEI - Mosquitos
Day
%
0.0
10.0
0.0
0.01
true
true
"" ""
PENS
"s" 0.04166667 0 -10899396 true "" ""
"e" 0.04166667 0 -1184463 true "" ""
"i" 0.04166667 0 -2674135 true "" ""

PLOT
1415
572
1716
722
SEITRM - People
Day
%
0.0
10.0
0.0
0.01
true
true
"" ""
PENS
"s" 0.04166667 0 -10899396 true "" ""
"e" 0.04166667 0 -1184463 true "" ""
"i" 0.04166667 0 -2674135 true "" ""
"t" 0.04166667 0 -5825686 true "" ""
"r" 0.04166667 0 -11221820 true "" ""
"m" 0.04166667 0 -13345367 true "" ""

MONITOR
955
94
1054
139
Real Time (sec)
sim-timer
1
1
11

SLIDER
20
412
307
445
Mosquito-Migration-Speed
Mosquito-Migration-Speed
0
20
4
.5
1
m/step
HORIZONTAL

SLIDER
640
290
929
323
Person-Migration-Speed
Person-Migration-Speed
0
10
7.5
.5
1
m/step
HORIZONTAL

SLIDER
641
334
927
367
Initial-Person-Immunity-Rate
Initial-Person-Immunity-Rate
0
100
0
5
1
%
HORIZONTAL

MONITOR
1415
727
1465
772
s
count people with [color = SUSCEPTIBLE]
17
1
11

MONITOR
1470
727
1520
772
e
(count people with [ color = EXPOSED])
17
1
11

MONITOR
1683
728
1733
773
m
(count people with [ color = IMMUNED])
17
1
11

MONITOR
1724
491
1781
536
i
(count mosquitos with [ color = INFECTIOUS])
17
1
11

SLIDER
1416
1478
1697
1511
Initial-Person-Infection-Rate
Initial-Person-Infection-Rate
0
10
0.5
0.1
1
%
HORIZONTAL

MONITOR
1725
387
1782
432
s
(count mosquitos with [ color = SUSCEPTIBLE])
17
1
11

MONITOR
2115
636
2246
681
Median Travel (m)
Meters-Per-Patch * median [sqrt((xcor - bornx) ^ 2 + (ycor - borny) ^ 2)] of mosquitos
2
1
11

SLIDER
19
501
307
534
Mosquito-House-Affinity
Mosquito-House-Affinity
0
100
90
1
1
%
HORIZONTAL

MONITOR
1256
95
1343
140
NIL
time-of-day\n
17
1
11

SLIDER
641
380
923
413
Person-Building-Affinity
Person-Building-Affinity
0
100
35
5
1
%
HORIZONTAL

BUTTON
1108
523
1264
556
NIL
stop-movie\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
949
523
1102
556
NIL
start-movie\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1426
803
1622
830
Infection Controls
20
0.0
1

CHOOSER
1417
1151
1698
1196
Initial-Infection-Distribution
Initial-Infection-Distribution
"none" "point-sources" "uniform-random"
1

SWITCH
948
618
1230
651
show-time?
show-time?
0
1
-1000

SWITCH
1417
1068
1640
1101
show-infection-sources?
show-infection-sources?
1
1
-1000

INPUTBOX
1416
1242
1699
1413
infection-seed-matrix
[2 0 0 100 0 10 1]
1
1
String

BUTTON
1420
846
1633
879
NIL
add-infection-sites
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
1420
888
1637
933
infection-seed-target
infection-seed-target
"mosquito" "human"
1

SLIDER
1421
942
1639
975
infection-seed-radius
infection-seed-radius
0
25
0.9
1
1
NIL
HORIZONTAL

SLIDER
1418
982
1637
1015
infection-seed-percentage
infection-seed-percentage
0
100
0
5
1
NIL
HORIZONTAL

SLIDER
1418
1023
1637
1056
infection-seed-strain
infection-seed-strain
0
4
1
1
1
NIL
HORIZONTAL

BUTTON
1416
1203
1696
1236
Click to apply change in selection
reload-infection-matrix\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
644
16
794
40
Layout Control
20
2.0
1

TEXTBOX
948
485
1159
510
Output Controls
20
0.0
1

CHOOSER
1117
828
1349
873
Patch-Color-Shows
Patch-Color-Shows
"none" "buildings" "ovi-dens" "wovi-dens" "host-dens" "whost-dens" "mosq-dens" "wolbachia-a" "wolbachia-f" "wolbachia-e" "wolbachia-l" "shade"
0

SLIDER
1116
1130
1350
1163
Diffusion-Rate
Diffusion-Rate
1
100
80
1
1
%
HORIZONTAL

SLIDER
1115
966
1349
999
Host-Dens-Max
Host-Dens-Max
.1
3
1.7
.1
1
NIL
HORIZONTAL

SLIDER
1117
1007
1349
1040
Mosq-Dens-Max
Mosq-Dens-Max
.1
5
2.5
.1
1
NIL
HORIZONTAL

SLIDER
1117
1046
1350
1079
Ovi-Dens-Max
Ovi-Dens-Max
10
200
90
10
1
NIL
HORIZONTAL

SWITCH
1197
884
1347
917
Draw-Palette?
Draw-Palette?
0
1
-1000

MONITOR
1577
728
1627
773
t
(count people with [ color = TEMPIMMUNE])
17
1
11

TEXTBOX
1119
797
1269
821
Field Controls:
20
0.0
1

BUTTON
1115
885
1190
918
Redraw
draw-patches\n
NIL
1
T
OBSERVER
NIL
D
NIL
NIL
1

TEXTBOX
956
405
1106
429
Checkpointing
20
0.0
1

BUTTON
953
444
1109
477
Checkpoint: write
export-world Run-Name
NIL
1
T
OBSERVER
NIL
W
NIL
NIL
1

BUTTON
1114
444
1269
477
Checkpoint: read
import-world user-file
NIL
1
T
OBSERVER
NIL
R
NIL
NIL
1

TEXTBOX
957
277
1168
325
Random Seed\n(0 for DO NOT SET)
20
0.0
1

INPUTBOX
957
337
1118
397
Random-Seed-Value
1
1
0
Number

SLIDER
1116
1171
1351
1204
Evaporation-Time
Evaporation-Time
0
24
10
1
1
hours
HORIZONTAL

SLIDER
642
526
927
559
Breteau-Index
Breteau-Index
0
300
200
10
1
NIL
HORIZONTAL

BUTTON
1032
1277
1183
1310
Draw mosq-dens
set Patch-Color-Shows \"mosq-dens\"\ndraw-patches
NIL
1
T
OBSERVER
NIL
M
NIL
NIL
1

BUTTON
1031
1328
1183
1361
Draw ovi-dens
set Patch-Color-Shows \"ovi-dens\"\ndraw-patches
NIL
1
T
OBSERVER
NIL
O
NIL
NIL
1

BUTTON
1031
1376
1184
1409
Draw host-dens
set Patch-Color-Shows \"host-dens\"\ndraw-patches
NIL
1
T
OBSERVER
NIL
H
NIL
NIL
1

SLIDER
1707
904
1925
937
H-to-V-Transmissibility
H-to-V-Transmissibility
0
1
0.9
.01
1
NIL
HORIZONTAL

SLIDER
1707
951
1920
984
V-to-H-Transmissibility
V-to-H-Transmissibility
0
1
0.9
.01
1
NIL
HORIZONTAL

BUTTON
1035
1429
1186
1462
Draw buildings
set Patch-Color-Shows \"buildings\"\ndraw-patches
NIL
1
T
OBSERVER
NIL
B
NIL
NIL
1

SLIDER
1116
1087
1350
1120
Ovisite-Diffusion
Ovisite-Diffusion
0
20
10
1
1
NIL
HORIZONTAL

SLIDER
644
51
839
84
Meters-Per-Patch
Meters-Per-Patch
.5
20
5
.5
1
NIL
HORIZONTAL

MONITOR
646
96
756
141
House-Width
hwidth
0
1
11

INPUTBOX
954
197
1227
257
Run-Name
test
1
0
String

BUTTON
1240
48
1303
81
End
all-stop
NIL
1
T
OBSERVER
NIL
E
NIL
NIL
1

TEXTBOX
334
233
578
258
Inputs: Eggs / Larvae
20
64.0
1

MONITOR
1060
94
1164
139
Real Time (min)
sim-timer / 60
2
1
11

BUTTON
66
1738
183
1771
Infect Agents
output-print \"Begin: <infect-agents>\"\ninfect-agents\noutput-print \"End: <infect-agents>\"
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
1417
1110
1644
1143
Infection-Day-Start
Infection-Day-Start
0
300
30
1
1
NIL
HORIZONTAL

BUTTON
1034
1483
1188
1516
Draw wovi-dens
set Patch-Color-Shows \"wovi-dens\"\ndraw-patches
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
1115
925
1347
958
Wovi-Dens-Max
Wovi-Dens-Max
0
5
2.6
.1
1
NIL
HORIZONTAL

SWITCH
1261
157
1378
190
view-on?
view-on?
0
1
-1000

SWITCH
1261
215
1384
248
plots-on?
plots-on?
0
1
-1000

INPUTBOX
1314
274
1390
334
Run-Number
0
1
0
Number

SLIDER
18
589
302
622
Wiggle-Size
Wiggle-Size
0
90
70
5
1
degrees
HORIZONTAL

SWITCH
22
546
302
579
Wiggle-On?
Wiggle-On?
0
1
-1000

SLIDER
325
50
610
83
Male-Mosquito-Ratio
Male-Mosquito-Ratio
0
100
50
5
1
%
HORIZONTAL

MONITOR
1795
146
1870
191
males
count males
17
1
11

SLIDER
328
91
610
124
Mating-Radius
Mating-Radius
0
10
1
.5
1
m
HORIZONTAL

PLOT
1859
1282
2229
1458
Percentages
Day
%
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"Mated" 0.04166667 0 -16777216 true "" ""
"Wolb-Fem" 0.04166667 0 -2674135 true "" ""
"Wolb-Mal" 0.04166667 0 -13345367 true "" ""

TEXTBOX
330
11
597
38
Inputs: Male Mosquitos
20
5.0
1

SLIDER
327
141
611
174
Mating-Probability
Mating-Probability
0
50
10
1
1
%
HORIZONTAL

PLOT
1858
1071
2231
1259
Age of Unmated Females
Day
Age (days)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Mean" 0.04166667 0 -2674135 true "" ""
"Median" 0.04166667 0 -13345367 true "" ""
"Max" 0.04166667 0 -1184463 true "" ""

MONITOR
2124
1592
2240
1637
Wolb. Female %
100 * count mosquitos with [wolbachia?] / count mosquitos
2
1
11

SWITCH
1707
850
1920
883
Propagate-Infection?
Propagate-Infection?
1
1
-1000

SWITCH
1237
355
1391
388
Header-Files?
Header-Files?
1
1
-1000

PLOT
1993
871
2227
1045
Bites
Bites
#
0.0
5.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

PLOT
1798
381
2119
562
Travel Profile
Age (days)
Mean Travel (m)
0.0
25.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" ""

SLIDER
22
459
301
492
Gonotrophic-Cycle-Length
Gonotrophic-Cycle-Length
0
5
3
.5
1
days
HORIZONTAL

SLIDER
328
596
608
629
Eclose-Time
Eclose-Time
0
30
14
1
1
days
HORIZONTAL

SLIDER
15
201
304
234
Mosquito-Mortality-Rate-Max
Mosquito-Mortality-Rate-Max
0
15
11
.5
1
%/day
HORIZONTAL

SLIDER
328
475
603
508
Egg-Hatch-Time
Egg-Hatch-Time
0
20
4
.5
1
days
HORIZONTAL

MONITOR
1794
197
1868
242
larvae
sum [n-larvae + n-larvae-w] of (patches with [ovi-site?])
0
1
11

MONITOR
1793
249
1867
294
eggs
sum [n-eggs + n-eggs-w] of (patches with [ovi-site?])
0
1
11

PLOT
1880
171
2187
356
Larva Ages
Age (days)
log(#)
0.0
25.0
1.5
4.0
true
false
"" ""
PENS
"age" 1.0 1 -2674135 true "" ""
"initial" 1.0 0 -13345367 true "" ""

PLOT
1879
14
2079
164
Egg Ages
NIL
NIL
0.0
10.0
0.0
5.0
true
false
"" ""
PENS
"age" 1.0 1 -2674135 true "" ""
"initial" 1.0 0 -13345367 true "" ""

BUTTON
1279
442
1407
475
Read Defaults
read-command-file \"CLARA.defaults\"\n
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
327
351
609
384
Egg-Mortality-Rate
Egg-Mortality-Rate
0
50
1
1
1
%/day
HORIZONTAL

SLIDER
326
263
603
296
Backfill-Eggs-Day
Backfill-Eggs-Day
0
30
5
1
1
NIL
HORIZONTAL

SLIDER
327
311
608
344
Level-Larvae-Keep-Ratio
Level-Larvae-Keep-Ratio
0
100
70
5
1
%
HORIZONTAL

SLIDER
2269
668
2549
701
Wolbachia-Release-Start-Day
Wolbachia-Release-Start-Day
0
196
98
7
1
NIL
HORIZONTAL

PLOT
2276
399
2593
592
Wolbachia Penetration
Day
%
0.0
10.0
0.0
0.01
true
true
"" ""
PENS
"Adult-M" 0.04166667 0 -7500403 true "" ""
"Adult-F" 0.04166667 0 -2674135 true "" ""
"Eggs" 0.04166667 0 -1184463 true "" ""
"Larvae" 0.04166667 0 -10899396 true "" ""
"Sterile" 0.04166667 0 -11221820 true "" ""

BUTTON
1035
1537
1193
1570
Draw Wolbachia-F
set Patch-Color-Shows \"wolbachia-f\"\nupdate-fields\ndraw-patches
NIL
1
T
OBSERVER
NIL
F
NIL
NIL
1

BUTTON
1034
1594
1193
1627
Draw Wolbachia-E
set Patch-Color-Shows \"wolbachia-e\"\nupdate-fields\ndraw-patches\n
NIL
1
T
OBSERVER
NIL
E
NIL
NIL
1

BUTTON
1035
1651
1192
1684
Draw Wolbachia-L
set Patch-Color-Shows \"wolbachia-l\"\nupdate-fields\ndraw-patches
NIL
1
T
OBSERVER
NIL
L
NIL
NIL
1

BUTTON
1205
1276
1324
1309
Hide Agents
set all-hidden? true\nset wolbachia-hidden? true\nask turtles [set hidden? true]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1203
1324
1343
1358
UnHide Agents
set all-hidden? false\nask mosquitos [set hidden? false]\nask males [set hidden? false]\nask people [set hidden? false]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1203
1366
1365
1399
UnHide Wolbachia
set wolbachia-hidden? false\nask mosquitos with [wolbachia?] [set hidden? false]\nask males with [wolbachia?] [set hidden? false]\n
NIL
1
T
OBSERVER
NIL
U
NIL
NIL
1

BUTTON
1205
1415
1364
1448
Draw Wolbachia-A
set Patch-Color-Shows \"wolbachia-a\"\nupdate-fields\ndraw-patches
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
1118
1220
1362
1253
Wolbachia-Patch-Aggregation
Wolbachia-Patch-Aggregation
1
20
5
1
1
NIL
HORIZONTAL

BUTTON
1206
1467
1318
1500
Draw NONE
set Patch-Color-Shows \"none\"\nask patches [set pcolor black]
NIL
1
T
OBSERVER
NIL
N
NIL
NIL
1

SLIDER
2269
747
2552
780
Wolbachia-Seed-Adults-per-house
Wolbachia-Seed-Adults-per-house
0
50
14
1
1
NIL
HORIZONTAL

SLIDER
327
392
583
425
Eggs-Per-Brood
Eggs-Per-Brood
1
120
24
1
1
NIL
HORIZONTAL

MONITOR
760
96
871
141
NIL
nhouses
0
1
11

MONITOR
760
149
871
194
Fem / House (tot)
count mosquitos / nhouses
1
1
11

MONITOR
760
201
873
246
Females / Human
count mosquitos / count people
1
1
11

MONITOR
646
200
758
245
Larvae / House
sum [n-larvae + n-larvae-w] of (patches with [ovi-site?]) / nhouses
1
1
11

MONITOR
646
149
754
194
Eggs / House
sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]) / nhouses
1
1
11

SLIDER
327
556
570
589
Straight-Travel-After-Ovi
Straight-Travel-After-Ovi
0
25
5
1
1
m
HORIZONTAL

SLIDER
326
434
573
467
Min-Eggs-Per-Oviposit
Min-Eggs-Per-Oviposit
0
25
10
5
1
NIL
HORIZONTAL

BUTTON
1186
10
1303
43
verbose
;foreach (list 100) [\nask one-of (patches with [ovi-site?]) [set pverbose? true]\n;]
NIL
1
T
OBSERVER
NIL
V
NIL
NIL
1

SLIDER
326
517
605
550
Age-Dep-Fecundity-Reduction
Age-Dep-Fecundity-Reduction
0
5
0
.1
1
%/day
HORIZONTAL

SLIDER
2268
784
2551
817
Wolb-Affect-Egg-Mortality-Rate
Wolb-Affect-Egg-Mortality-Rate
0
500
0
50
1
%
HORIZONTAL

SLIDER
2267
821
2572
854
Wolb-Affect-Larva-Mortality-Rate
Wolb-Affect-Larva-Mortality-Rate
0
50
0
5
1
%
HORIZONTAL

MONITOR
1857
1489
1962
1534
mean Mu-max
mean [mu-max] of patches with [ovi-site?]
2
1
11

MONITOR
1857
1540
1965
1585
median Mu-max
median [mu-max] of patches with [ovi-site?]
2
1
11

MONITOR
1858
1592
1962
1637
stdev Mu-max
standard-deviation [mu-max] of patches with [ovi-site?]
2
1
11

MONITOR
1975
1490
2085
1535
mean ovi-dens
mean [ovi-dens] of patches with [ovi-site?]
3
1
11

MONITOR
1975
1544
2096
1589
median ovi-dens
median [ovi-dens] of patches with [ovi-site?]
3
1
11

MONITOR
1975
1594
2084
1639
stdev ovi-dens
standard-deviation [ovi-dens] of patches with [ovi-site?]
3
1
11

MONITOR
1314
10
1407
55
sec / sim-day
sim-timer / sim-day
1
1
11

SLIDER
2267
858
2571
891
Wolb-Affect-Adult-Mortality-Rate
Wolb-Affect-Adult-Mortality-Rate
0
100
10
5
1
%
HORIZONTAL

SLIDER
2270
631
2549
664
Wolbachia-Sites-Every-Nth
Wolbachia-Sites-Every-Nth
0
16
4
1
1
NIL
HORIZONTAL

SLIDER
644
569
905
602
K-Per-House
K-Per-House
0
50
33.75
.25
1
NIL
HORIZONTAL

SLIDER
328
638
448
671
Omega
Omega
0
2
1
.1
1
NIL
HORIZONTAL

SLIDER
465
637
579
670
Sigma
Sigma
0
5
2.5
.5
1
%
HORIZONTAL

MONITOR
2088
65
2146
110
e-idx
array:item egg-index 0
0
1
11

MONITOR
2089
116
2147
161
l-idx
array:item larva-index 0
0
1
11

MONITOR
882
201
939
246
L / M
sum [n-larvae + n-larvae-w] of (patches with [ovi-site?]) / (count mosquitos + count males)
1
1
11

MONITOR
882
150
941
195
L / K / h
sum [n-larvae + n-larvae-w] of (patches with [ovi-site?]) / K-Per-House / nhouses\n
1
1
11

TEXTBOX
647
608
935
638
Scaling: (Adult Females / Prop.) * 4.8 = K / H
12
0.0
1

MONITOR
881
100
938
145
E / M
sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]) / (count mosquitos + count males)
1
1
11

TEXTBOX
2271
602
2491
624
Wolbachia Controls
20
0.0
1

SWITCH
645
636
825
669
Heterogenous-K?
Heterogenous-K?
1
1
-1000

TEXTBOX
648
481
798
505
Input: Houses
20
0.0
1

BUTTON
1210
1649
1346
1682
NIL
dump-ovi-dens
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
2269
708
2550
741
Wolbachia-Release-End-Day
Wolbachia-Release-End-Day
0
266
161
7
1
NIL
HORIZONTAL

BUTTON
33
853
147
887
NIL
color-patch
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
158
843
297
888
color-to-use
color-to-use
0 9.9 5 55 25
0

BUTTON
308
853
427
887
dump-roads
file-open \"roads.dat\"\nask patches with [pcolor = white] [\n  file-print (word pxcor \" \" pycor)\n]\nfile-close\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
437
854
549
888
NIL
read-roads
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
2268
1167
2552
1212
Wolbachia-Release-On
Wolbachia-Release-On
"ovi-sites" "front-doors" "roads"
1

CHOOSER
559
845
809
890
roads-file
roads-file
"roads-serpentine.dat" "roads-blocks.dat" "roads-all-cross-streets.dat"
0

SWITCH
329
680
587
713
use-random-binomial?
use-random-binomial?
0
1
-1000

SWITCH
957
663
1226
696
write-data?
write-data?
0
1
-1000

INPUTBOX
640
420
930
480
Input-File-Root
inputs/500-tight
1
0
String

SLIDER
642
679
910
712
Outdoor-Ovisite-Number-Ratio
Outdoor-Ovisite-Number-Ratio
0
5
0.5
.1
1
NIL
HORIZONTAL

SLIDER
643
728
913
761
Outdoor-Ovisite-K-Ratio-Max
Outdoor-Ovisite-K-Ratio-Max
0
5
2
.1
1
NIL
HORIZONTAL

SLIDER
2267
942
2559
975
Wolb-Affect-Mating-Probability
Wolb-Affect-Mating-Probability
0
100
0
5
1
%
HORIZONTAL

SLIDER
2269
991
2527
1024
Wolb-Affect-Leakage-Rate
Wolb-Affect-Leakage-Rate
0
100
0
1
1
%
HORIZONTAL

SLIDER
647
776
819
809
Inflate-K
Inflate-K
.5
3
2
.1
1
NIL
HORIZONTAL

SLIDER
2270
1038
2468
1071
Hurricane-Start-Day
Hurricane-Start-Day
0
300
0
7
1
NIL
HORIZONTAL

SLIDER
2268
1078
2460
1111
Hurricane-Kill-Ratio
Hurricane-Kill-Ratio
0
1
0.6
.05
1
NIL
HORIZONTAL

SLIDER
952
708
1227
741
Void-Border-Width
Void-Border-Width
0
200
25
5
1
m
HORIZONTAL

BUTTON
1218
1532
1340
1565
draw-shade
set Patch-Color-Shows \"shade\"\ndraw-patches\n
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
950
754
1232
787
Outdoor-Ovisite-Border-Width
Outdoor-Ovisite-Border-Width
0
200
50
5
1
m
HORIZONTAL

SWITCH
2270
1125
2544
1158
Clear-Wolbachia-On-Migration?
Clear-Wolbachia-On-Migration?
1
1
-1000

MONITOR
851
42
939
87
Fem / Prop.
count (mosquitos-on village-patches) / nproperties
1
1
11

SLIDER
2266
896
2548
929
Wolb-Affect-Fecundity
Wolb-Affect-Fecundity
0
100
10
5
1
%
HORIZONTAL

SLIDER
2272
1233
2558
1266
Wolb-Affect-Fitness-Cost
Wolb-Affect-Fitness-Cost
0
100
10
5
1
%
HORIZONTAL

SLIDER
1234
663
1406
696
Equilibrium-Day
Equilibrium-Day
0
100
95
1
1
NIL
HORIZONTAL

SLIDER
2275
1280
2561
1313
Wolbachia-Release-Frequency
Wolbachia-Release-Frequency
0
50
7
1
1
days
HORIZONTAL

BUTTON
573
1210
699
1243
NIL
fix-mosquitos
NIL
1
T
OBSERVER
NIL
X
NIL
NIL
1

BUTTON
570
1269
726
1302
show odd agents
set all-hidden? true\nset wolbachia-hidden? true\nask turtles [set hidden? true]\nask mosquitos with [color != green] [set hidden? false]\nask males with [color != gray] [set hidden? false]\n
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
2278
1329
2560
1362
Wolb-Affect-Vectorial-Capacity
Wolb-Affect-Vectorial-Capacity
0
100
0
5
1
%
HORIZONTAL

BUTTON
570
1501
740
1535
setup infection run
read-checkpoint\nsetup-infection-run\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
2273
204
2530
391
Vector Capable
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 0.04166667 0 -2674135 true "" ""

SWITCH
1417
1579
1695
1612
log-mosquito-infections?
log-mosquito-infections?
1
1
-1000

SWITCH
1237
753
1400
786
write-mig-file?
write-mig-file?
1
1
-1000

SWITCH
1237
711
1402
744
write-mate-file?
write-mate-file?
1
1
-1000

INPUTBOX
1194
274
1299
334
Run-Number-Start
1
1
0
Number

SLIDER
2274
1640
2571
1673
Wolb-Pre-Release-Adult-Suppression
Wolb-Pre-Release-Adult-Suppression
0
100
0
5
1
%
HORIZONTAL

SLIDER
2275
1687
2572
1720
Wolb-Pre-Release-Larval-Suppression
Wolb-Pre-Release-Larval-Suppression
0
100
0
5
1
%
HORIZONTAL

SWITCH
2273
1386
2543
1419
Use-Wolb-Affect-Fitness-Cost?
Use-Wolb-Affect-Fitness-Cost?
1
1
-1000

TEXTBOX
2278
1608
2428
1632
Interventions
20
15.0
1

SLIDER
2277
1742
2571
1775
Vaccine-Campaign-Start
Vaccine-Campaign-Start
0
250
210
7
1
day
HORIZONTAL

SLIDER
2276
1795
2570
1828
Vaccine-Campaign-Duration
Vaccine-Campaign-Duration
0
500
0
7
1
days
HORIZONTAL

SLIDER
2277
1844
2570
1877
Vaccine-Campaign-Coverage
Vaccine-Campaign-Coverage
0
100
50
1
1
%
HORIZONTAL

SLIDER
2277
1894
2571
1927
Vaccine-Campaign-Age-Min
Vaccine-Campaign-Age-Min
0
100
0
1
1
years
HORIZONTAL

SLIDER
2278
1950
2575
1983
Vaccine-Campaing-Age-Max
Vaccine-Campaing-Age-Max
0
100
15
1
1
years
HORIZONTAL

SWITCH
891
834
1062
867
is-vector-control?
is-vector-control?
0
1
-1000

TEXTBOX
892
796
1042
821
Vector Controls
20
0.0
1

SWITCH
891
875
1063
908
is-reduction-control?
is-reduction-control?
0
1
-1000

SWITCH
891
916
1064
949
is-larvicide-control?
is-larvicide-control?
0
1
-1000

SLIDER
891
956
1064
989
control-init-day
control-init-day
0
150
113
1
1
NIL
HORIZONTAL

SLIDER
891
997
1065
1030
reduction-init-day
reduction-init-day
0
150
113
1
1
NIL
HORIZONTAL

SLIDER
891
1039
1065
1072
larvicide-init-day
larvicide-init-day
0
150
113
1
1
NIL
HORIZONTAL

TEXTBOX
662
926
812
951
Seasonality
20
0.0
1

SLIDER
661
960
834
993
peak-day
peak-day
0
150
126
1
1
NIL
HORIZONTAL

TEXTBOX
663
1036
813
1061
Symptomaticity
20
0.0
1

SLIDER
662
1070
836
1103
symptomatic-ratio
symptomatic-ratio
0
100
25
1
1
%
HORIZONTAL

SLIDER
891
1080
1065
1113
cover-radius
cover-radius
0
500
100
5
1
m
HORIZONTAL

SLIDER
891
1122
1073
1155
control-percent-eggs
control-percent-eggs
0
100
60
1
1
%
HORIZONTAL

SLIDER
891
1163
1081
1196
control-percent-larvae
control-percent-larvae
0
100
60
1
1
%
HORIZONTAL

SLIDER
891
1205
1096
1238
control-percent-mosquito
control-percent-mosquito
0
100
80
1
1
%
HORIZONTAL

BUTTON
66
1679
179
1712
Setup-Apollo
output-print \"Begin: <Setup-Apollo>\"\nset Run-Name \"Apollo\"\nno-display\nset view-on? false\nset plots-on? false\nread-command-file \"config.txt\"\nsetup\noutput-print \"End: <Setup-Apollo>\"
NIL
1
T
OBSERVER
NIL
A
NIL
NIL
1

SWITCH
1236
619
1405
652
write-bite-file?
write-bite-file?
1
1
-1000

SWITCH
1236
577
1405
610
write-pop-file?
write-pop-file?
1
1
-1000

SWITCH
945
578
1101
611
write-tprof-file?
write-tprof-file?
1
1
-1000

@#$#@#$#@
## Version

This version taken from CLARA_vc_rc_lc_season-NTBS.nlogo on 2014.09.09

## TO DO

* explicit seasonality (via water levels?)
  - possible means: vary the Container-Index (CI), vary the Carrying-Capacity (K)  
* use actual house layout/spacing  
* change egg/larva init to discrete integer, not floating point

---- maybe:

* compare to transmissibility figures from literature  
* lower the EIP to <= gonotrophic cycle length ??? (90% of the time they can infect after one cycle)  
* give mosquitos body weight, and make their mortality/fecundity depend on it  
* state-dependent mosquito step speed  
- slow down or stop to digest blood meal  
* mosquitos don't like "open country"  
* variable egg # (more in the first cycle, fewer thereafter)  
* explore heterogeneity (use Cairns ovitrap data)

## ON INPUT PARAMETERS

excerpt from the VECNet Statement of Work:  
* "The common  model inputs for vectors are the key bionomic parameters identified by the VECNet vector biologists as critical drivers of transmission, namely the adult life expectancy, the proportion of blood meals on different hosts, the length of the gonotropic cycle, the egg batch size, the nature of the larval habitat, the length of the immature stages, immature survivorship, endophagy, the sporozoite and inoculation rates, the biting density, the time of blood feeding, the location of resting sites and sugar feeding frequency."

## CAIRNS DATA

* for Yorkey's Knob: set Inflate-K=1 and K-Per-House=33.75 to get ~ 7 Females/property  
* for Gordonvale: set Inflate-K=2 and K-Per-House=33.75 to get ~ 14 Females/property

## LARVAE

* average larval density in Iquitos studies were 30 larvae/L  
* and... "Laboratory assays have demonstrated a dose-specific oviposition response that increased with conspecific densities up to 1000 larva/L and decreased thereafter"
  ["Oviposition Site Selection by the Dengue Vector Aedes aegypti and Its Implications for Dengue Control" Jacklyn Wong, Steven T. Stoddard, Helvio Astete, Amy C. Morrison, Thomas W. Scott]
  - for our purposes, assuming a 10 gal (40L) basin as the typical ovisite, that makes 1200 larvae/container, repulsive @ 10,000 larvae/cont.

## QUESTIONS

* RE: Mating, how do males move?  Would they just random-walk around the ovi-site where they were born?

## OTHER NOTES

* figure out a way to use this germ to center the distribution, and track its spread  
total distance, max-distance, weighted-distance, people-distance, mosquito-distance, etc.

*consider tracking person-travel AND mosquito-travel and increment it every time we infect someone... (!)

* track movement history (a list of x,y,m/h) for every infection  
draw a line on screen whenever an infection is made (red/blue for m/h)

* track infection count (passed down):
 -- -1 for never infected
 -- 0 for never passed along (these are the "edge" cases)
 -- >0 for how many you infected

     this could help get at an effective R0



## BRETEAU INDEX (BI)

The number of positive containers (i.e. containing Aedes aegypti larvae) per 100 premises inspected. When it is 50 or more then the risk of transmission is high and when it is 5 then risk of transmission is low.  Some sites [???] have BI measured as high as 300 - 400. 

## HOUSE INDEX (HI)

The percentage of houses infested with larvae and/or pupae with one or more habitats for Aedes aegypti or related species.

## CONTAINER INDEX (CI)

The percentage of water holding containers infested with larvae and/or pupae.

## PUPAL INDEX (PI)

The emergence of adult mosquito population can be estimated by the pupal count (i.e., by counting all pupae found in each container. The corresponding index is the Pupal Index (PI), i.e., the number of pupae per 100 houses.

## WHAT IS IT?

This model simulates both mosquitos and people in the same geographic area, in the presence of 4 strains of the Dengue virus.  It models disease propagation and dynamics.

## HOW IT WORKS

Dengue is propagated to people from mosquitos and to mosquitos from people (no intra-species spreading) during a mosquito bite.  

* Mosquitos only bite when they are "hungry" (adjust the Time-Between-Bites slider).  
* Mosquitos (all assumed to be female) only breed every 7th day, and must have a "blood-meal" in order to lay a brood.

## HOW TO USE IT

Press "Setup" then "Go".  Watch the agents to their thing.

## POPULATION DYNAMICS

The human population is static.  The mosquitos, however, are being born (adjust the Mosquitos-Per-Brood slider) and dying (adjust the Mosquito-Mortality-Rate slider) all the time.  The stability of the mosquito population is sensitively dependent upon these two sliders.

## THINGS TO NOTICE

Color codes:  
Blue
       = Susceptible

Red
        = Exposed

Yellow
     = Infectious

Light Blue = susceptible, but recovered from a previous infection  
Green
      = Recovered from the second infection, and therefore immune to all strains


## THINGS TO TRY

* See how the ultimate "Infected Ratios" depend on initial infection rates.  
* See how geographic propagation depends on mosquito/human travel.
  (yes, I know humans don't travel yet...)

## EXTENDING THE MODEL

* NOTE: variables with Mixed-Capitals style refer to buttons/sliders on the Interface tab.  
* make people move  
* build "households" of "families"

## RELATED MODELS

I learned a few things by studying the AIDS model (under Sample Models --> Social Science).

## CREDITS AND REFERENCES

Nathan Stone (PSC), Shawn Brown (PSC/PITT), Derek Cummings (JHU)

* Flight potential  
["Aedes aegypti: size, reserves, survival, and flight potential", Briegel, Knusel, Timmermann, 2001]  
- maximum distances (lab, not field) 11-18 km  
- continuous flight: 2-9 hours (mean: 2.2 hours)  
- average flight speed: 640 m/s (poor: 470 m/s, vigorous: 1000 m/s)
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

caterpillar
true
0
Polygon -7500403 true true 165 210 165 225 135 255 105 270 90 270 75 255 75 240 90 210 120 195 135 165 165 135 165 105 150 75 150 60 135 60 120 45 120 30 135 15 150 15 180 30 180 45 195 45 210 60 225 105 225 135 210 150 210 165 195 195 180 210
Line -16777216 false 135 255 90 210
Line -16777216 false 165 225 120 195
Line -16777216 false 135 165 180 210
Line -16777216 false 150 150 201 186
Line -16777216 false 165 135 210 150
Line -16777216 false 165 120 225 120
Line -16777216 false 165 106 221 90
Line -16777216 false 157 91 210 60
Line -16777216 false 150 60 180 45
Line -16777216 false 120 30 96 26
Line -16777216 false 124 0 135 15

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

hawk
true
0
Polygon -7500403 true true 151 170 136 170 123 229 143 244 156 244 179 229 166 170
Polygon -16777216 true false 152 154 137 154 125 213 140 229 159 229 179 214 167 154
Polygon -7500403 true true 151 140 136 140 126 202 139 214 159 214 176 200 166 140
Polygon -16777216 true false 151 125 134 124 128 188 140 198 161 197 174 188 166 125
Polygon -7500403 true true 152 86 227 72 286 97 272 101 294 117 276 118 287 131 270 131 278 141 264 138 267 145 228 150 153 147
Polygon -7500403 true true 160 74 159 61 149 54 130 53 139 62 133 81 127 113 129 149 134 177 150 206 168 179 172 147 169 111
Circle -16777216 true false 144 55 7
Polygon -16777216 true false 129 53 135 58 139 54
Polygon -7500403 true true 148 86 73 72 14 97 28 101 6 117 24 118 13 131 30 131 22 141 36 138 33 145 72 150 147 147

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

monster
false
0
Polygon -7500403 true true 75 150 90 195 210 195 225 150 255 120 255 45 180 0 120 0 45 45 45 120
Circle -16777216 true false 165 60 60
Circle -16777216 true false 75 60 60
Polygon -7500403 true true 225 150 285 195 285 285 255 300 255 210 180 165
Polygon -7500403 true true 75 150 15 195 15 285 45 300 45 210 120 165
Polygon -7500403 true true 210 210 225 285 195 285 165 165
Polygon -7500403 true true 90 210 75 285 105 285 135 165
Rectangle -7500403 true true 135 165 165 270

moon
false
0
Polygon -7500403 true true 175 7 83 36 25 108 27 186 79 250 134 271 205 274 281 239 207 233 152 216 113 185 104 132 110 77 132 51

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

sun
false
0
Circle -7500403 true true 75 75 150
Polygon -7500403 true true 300 150 240 120 240 180
Polygon -7500403 true true 150 0 120 60 180 60
Polygon -7500403 true true 150 300 120 240 180 240
Polygon -7500403 true true 0 150 60 120 60 180
Polygon -7500403 true true 60 195 105 240 45 255
Polygon -7500403 true true 60 105 105 60 45 45
Polygon -7500403 true true 195 60 240 105 255 45
Polygon -7500403 true true 240 195 195 240 255 255

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

warning
false
0
Polygon -7500403 true true 0 240 15 270 285 270 300 240 165 15 135 15
Polygon -16777216 true false 180 75 120 75 135 180 165 180
Circle -16777216 true false 129 204 42

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
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="breteau" repetitions="1" runMetricsEveryStep="true">
    <setup>set Run-Name "breteau"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Breteau-Index">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
      <value value="250"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="mos-ratio" repetitions="1" runMetricsEveryStep="true">
    <setup>set Run-Name "mos-ratio"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <steppedValueSet variable="Initial-Mosquito-to-Human-Ratio" first="2" step="2" last="20"/>
  </experiment>
  <experiment name="infections-nocp" repetitions="1" runMetricsEveryStep="true">
    <setup>no-display
set view-on? false
set plots-on? false
set Propagate-Infection? true
set Max-Days 200
set Run-Name "production-run"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <metric>count mosquitos with [color = EXPOSED]</metric>
    <metric>count mosquitos with [color = INFECTIOUS]</metric>
    <metric>count people with [color = EXPOSED]</metric>
    <metric>count people with [color = INFECTIOUS]</metric>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="16"/>
  </experiment>
  <experiment name="wiggle" repetitions="1" runMetricsEveryStep="true">
    <setup>set Run-Name "wiggle"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <steppedValueSet variable="Wiggle-Size" first="10" step="10" last="90"/>
  </experiment>
  <experiment name="mating-probability" repetitions="1" runMetricsEveryStep="true">
    <setup>set Run-Name "mating-probability"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Mating-Probability">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="7.5"/>
      <value value="10"/>
      <value value="12.5"/>
      <value value="15"/>
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Mating-Radius" first="0.5" step="0.5" last="4"/>
  </experiment>
  <experiment name="biting" repetitions="1" runMetricsEveryStep="true">
    <setup>set Run-Name "biting"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <enumeratedValueSet variable="Mosquito-Migration-Speed">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Bite-Radius" first="2" step="2" last="10"/>
  </experiment>
  <experiment name="single" repetitions="1" runMetricsEveryStep="true">
    <setup>set Run-Name "single"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <metric>count mosquitos with [color = EXPOSED]</metric>
    <metric>count mosquitos with [color = INFECTIOUS]</metric>
    <metric>count people with [color = EXPOSED]</metric>
    <metric>count people with [color = INFECTIOUS]</metric>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="1"/>
  </experiment>
  <experiment name="multi-seed" repetitions="1" runMetricsEveryStep="false">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "multi-seed"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <metric>count mosquitos with [wolbachia?]</metric>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="3"/>
  </experiment>
  <experiment name="wolbachia" repetitions="1" runMetricsEveryStep="true">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "wolbachia"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <metric>100 * count mosquitos with [wolbachia?] / count mosquitos</metric>
    <metric>100 * (sum [n-eggs-w] of (patches with [ovi-site?])) / (sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]))</metric>
    <metric>100 * (sum [n-larvae-w] of (patches with [ovi-site?])) / (sum [n-larvae + n-larvae-w] of (patches with [ovi-site?]))</metric>
    <metric>100 * count mosquitos with [sterile?] / count mosquitos</metric>
    <enumeratedValueSet variable="Wolbachia-Seed-Adults">
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="wolbachia-duration-front-doors" repetitions="1" runMetricsEveryStep="true">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "wolbachia-duration-front-doors"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <metric>100 * count mosquitos with [wolbachia?] / count mosquitos</metric>
    <metric>100 * (sum [n-eggs-w] of (patches with [ovi-site?])) / (sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]))</metric>
    <metric>100 * (sum [n-larvae-w] of (patches with [ovi-site?])) / (sum [n-larvae + n-larvae-w] of (patches with [ovi-site?]))</metric>
    <metric>100 * count mosquitos with [sterile?] / count mosquitos</metric>
    <enumeratedValueSet variable="Wolbachia-Release-On">
      <value value="&quot;front-doors&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Wolbachia-Release-End-Day" first="98" step="7" last="168"/>
  </experiment>
  <experiment name="wolbachia-duration-ovi-sites" repetitions="1" runMetricsEveryStep="true">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "wolbachia-duration-ovi-sites"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <metric>100 * count mosquitos with [wolbachia?] / count mosquitos</metric>
    <metric>100 * (sum [n-eggs-w] of (patches with [ovi-site?])) / (sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]))</metric>
    <metric>100 * (sum [n-larvae-w] of (patches with [ovi-site?])) / (sum [n-larvae + n-larvae-w] of (patches with [ovi-site?]))</metric>
    <metric>100 * count mosquitos with [sterile?] / count mosquitos</metric>
    <enumeratedValueSet variable="Wolbachia-Release-On">
      <value value="&quot;ovi-sites&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Wolbachia-Release-End-Day" first="98" step="7" last="168"/>
  </experiment>
  <experiment name="wolbachia-duration-roads" repetitions="1" runMetricsEveryStep="true">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "wolbachia-duration-roads"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <metric>100 * count mosquitos with [wolbachia?] / count mosquitos</metric>
    <metric>100 * (sum [n-eggs-w] of (patches with [ovi-site?])) / (sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]))</metric>
    <metric>100 * (sum [n-larvae-w] of (patches with [ovi-site?])) / (sum [n-larvae + n-larvae-w] of (patches with [ovi-site?]))</metric>
    <metric>100 * count mosquitos with [sterile?] / count mosquitos</metric>
    <enumeratedValueSet variable="Wolbachia-Release-On">
      <value value="&quot;roads&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Wolbachia-Release-End-Day" first="98" step="7" last="168"/>
  </experiment>
  <experiment name="wolbachia-seed" repetitions="1" runMetricsEveryStep="true">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "wolbachia-seed"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>count mosquitos</metric>
    <metric>100 * count mosquitos with [wolbachia?] / count mosquitos</metric>
    <metric>100 * (sum [n-eggs-w] of (patches with [ovi-site?])) / (sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]))</metric>
    <metric>100 * (sum [n-larvae-w] of (patches with [ovi-site?])) / (sum [n-larvae + n-larvae-w] of (patches with [ovi-site?]))</metric>
    <metric>100 * count mosquitos with [sterile?] / count mosquitos</metric>
    <enumeratedValueSet variable="Wolbachia-Release-On">
      <value value="&quot;front-doors&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Wolbachia-Seed-Adults-per-House" first="2" step="2" last="20"/>
  </experiment>
  <experiment name="spread-infection" repetitions="1" runMetricsEveryStep="false">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "spread-infection"
set Propagate-Infection? true
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Wolbachia-Release-Start-Day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolbachia-Release-End-Day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolbachia-Seed-Adults-per-House">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Infection-Day-Start">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-Person-Immunity-Rate">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="Max-Days">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Header-Files?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="input-sweep" repetitions="1" runMetricsEveryStep="false">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "input-sweep"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Wolbachia-Seed-Adults-per-House">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Input-File-Root">
      <value value="&quot;inputs/500-tight&quot;"/>
      <value value="&quot;inputs/Gordonvale&quot;"/>
      <value value="&quot;inputs/YorkeysKnob&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="outdoor-ovisite-sweep" repetitions="1" runMetricsEveryStep="false">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "outdoor-ovisite-sweep"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Outdoor-Ovisite-Number-Ratio">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
      <value value="1.1"/>
      <value value="1.2"/>
      <value value="1.5"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="wolb-affects" repetitions="1" runMetricsEveryStep="false">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "wolb-affects"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <steppedValueSet variable="Wolb-Affect-Mating-Probability" first="0" step="10" last="50"/>
    <steppedValueSet variable="Wolb-Affect-Leakage-Rate" first="0" step="5" last="25"/>
  </experiment>
  <experiment name="border" repetitions="1" runMetricsEveryStep="false">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "border"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Outdoor-Ovisite-Border-Width">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Void-Border-Width">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="k-values" repetitions="1" runMetricsEveryStep="false">
    <setup>no-display
set view-on? false
set plots-on? false
set Run-Name "k-values"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Inflate-K">
      <value value="0.9"/>
      <value value="1"/>
      <value value="1.1"/>
      <value value="1.4"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-Seed-Value">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="fitness" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "fitness"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Use-Wolb-Affect-Fitness-Cost?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Wolbachia-Release-End-Day" first="98" step="7" last="161"/>
    <enumeratedValueSet variable="Wolb-Affect-Fitness-Cost">
      <value value="0"/>
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="10"/>
  </experiment>
  <experiment name="density" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "density"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Heterogenous-K?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolbachia-Sites-Every-Nth">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="30"/>
  </experiment>
  <experiment name="frequency" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "frequency"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Heterogenous-K?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolbachia-Release-Frequency">
      <value value="3"/>
      <value value="7"/>
      <value value="10"/>
      <value value="14"/>
      <value value="21"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="30"/>
  </experiment>
  <experiment name="speed" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "speed"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <steppedValueSet variable="Mosquito-Migration-Speed" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="Heterogenous-K?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="3"/>
  </experiment>
  <experiment name="infections-0" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-Person-Immunity-Rate" first="0" step="20" last="80"/>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="infections-10" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-10&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-Person-Immunity-Rate" first="0" step="20" last="80"/>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="infections-20" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-20&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-Person-Immunity-Rate" first="0" step="20" last="80"/>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="infections-30" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-30&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-Person-Immunity-Rate" first="0" step="20" last="80"/>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="30"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="infections-40" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-40&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-Person-Immunity-Rate" first="0" step="20" last="80"/>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="40"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="infections-50" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-50&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-Person-Immunity-Rate" first="0" step="20" last="80"/>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="infections-60" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-60&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-Person-Immunity-Rate" first="0" step="20" last="80"/>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="60"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="infections-70" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-70&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-Person-Immunity-Rate" first="0" step="20" last="80"/>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="70"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="infections-80" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-80&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-Person-Immunity-Rate" first="0" step="20" last="80"/>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="80"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="infections-90" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-90&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Initial-Person-Immunity-Rate" first="0" step="20" last="80"/>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="90"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="infections-test" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;infections-0&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolb-affect-vectorial-capacity">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="2" step="1" last="2"/>
  </experiment>
  <experiment name="mortality" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "mortality"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <steppedValueSet variable="Wolb-Affect-Adult-Mortality-Rate" first="1" step="0.2" last="2"/>
  </experiment>
  <experiment name="suppression-fec10" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "suppression-fec10"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Wolb-Affect-Fecundity">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Affect-Adult-Mortality-Rate">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Larval-Suppression">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Adult-Suppression">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="suppression-fec20" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "suppression-fec20"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Wolb-Affect-Fecundity">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Affect-Adult-Mortality-Rate">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Larval-Suppression">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Adult-Suppression">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="suppression-fec30" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "suppression-fec30"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Wolb-Affect-Fecundity">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Affect-Adult-Mortality-Rate">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Larval-Suppression">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Adult-Suppression">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="suppression-fec50" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "suppression-fec50"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Wolb-Affect-Fecundity">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Affect-Adult-Mortality-Rate">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Larval-Suppression">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Adult-Suppression">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="suppression-fec0" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "suppression-fec0"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Wolb-Affect-Fecundity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Affect-Adult-Mortality-Rate">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Larval-Suppression">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Adult-Suppression">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="h2vtrans" repetitions="1" runMetricsEveryStep="false">
    <setup>write-input-file
read-checkpoint
read-input-file
setup-infection-run</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;h2vtrans&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="run-number-start">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H-to-V-Transmissibility">
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed-value" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="suppression" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "suppression"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Wolb-Pre-Release-Larval-Suppression">
      <value value="0"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Pre-Release-Adult-Suppression">
      <value value="0"/>
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="30"/>
  </experiment>
  <experiment name="release-on" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "release-on"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Wolbachia-Release-On">
      <value value="&quot;front-doors&quot;"/>
      <value value="&quot;ovi-sites&quot;"/>
      <value value="&quot;roads&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="10"/>
  </experiment>
  <experiment name="bite-radius" repetitions="1" runMetricsEveryStep="true">
    <setup>set Run-Name "bite-radius"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>sum [n-eggs + n-eggs-w] of (patches with [ovi-site?])</metric>
    <metric>sum [n-larvae + n-larvae-w] of (patches with [ovi-site?])</metric>
    <metric>count mosquitos</metric>
    <metric>count mosquitos with [color = SUSCEPTIBLE]</metric>
    <metric>count mosquitos with [color = EXPOSED]</metric>
    <metric>count mosquitos with [color = INFECTIOUS]</metric>
    <metric>count people with [color = SUSCEPTIBLE]</metric>
    <metric>count people with [color = EXPOSED]</metric>
    <metric>count people with [color = INFECTIOUS]</metric>
    <metric>count people with [color = TEMPIMMUNE]</metric>
    <metric>count people with [color = RECOVERED]</metric>
    <metric>sum [ovi-site-k] of (patches with [ovi-site?])</metric>
    <steppedValueSet variable="Bite-Radius" first="5" step="5" last="10"/>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="2"/>
    <enumeratedValueSet variable="Wiggle-On?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="speed_seasonality" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "speed"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <steppedValueSet variable="Mosquito-Migration-Speed" first="1" step="1" last="5"/>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="10"/>
  </experiment>
  <experiment name="fitness_seasonality" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "fitness"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <metric>100 * (sum [n-eggs-w] of (patches with [ovi-site?])) / (sum [n-eggs + n-eggs-w] of (patches with [ovi-site?]))</metric>
    <enumeratedValueSet variable="Use-Wolb-Affect-Fitness-Cost?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Wolb-Affect-Fitness-Cost">
      <value value="20"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Random-Seed-Value" first="1" step="1" last="1"/>
  </experiment>
  <experiment name="vector-control" repetitions="1" runMetricsEveryStep="false">
    <setup>set Run-Name "vcontrol1"
no-display
set view-on? false
set plots-on? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Wiggle-On?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Random-Seed-Value" first="6" step="1" last="10"/>
  </experiment>
  <experiment name="test" repetitions="1" runMetricsEveryStep="true">
    <setup>set Run-Name "test"
no-display
set view-on? false
set plots-on? false
set write-data? false
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
    <enumeratedValueSet variable="Run-Name">
      <value value="&quot;test&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-Seed-Value">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Run-Number-Start">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Max-Days">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Apollo" repetitions="1" runMetricsEveryStep="true">
    <setup>set Run-Name "Apollo"
no-display
set view-on? false
set plots-on? false
read-command-file "config.txt"
setup</setup>
    <go>go</go>
    <final>all-stop</final>
    <exitCondition>ticks &gt;= Max-Days * ticks-per-day</exitCondition>
  </experiment>
</experiments>
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
