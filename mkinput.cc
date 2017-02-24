/*
 * Author: Nathan Stone <stone@psc.edu>
 * 
 * command line usage: ./mkinput -c Gordonvale.cfg -u 
 */

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <err.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <sys/param.h>

#include <list>
using namespace std;

// TYPES --------------------------------------------------

typedef struct pdf {
  int min;
  int max;
  int len;
  float median;
  float *val;
} pdf_t;

typedef struct coordinates {
  float x;
  float y;
} coord_t;

typedef struct icoordinates {
  int x;
  int y;
} icoord_t;

typedef struct location {
  int id;
  int type;
  coord_t coord;
  int capacity;
  int openings;
} loc_t;

typedef struct person {
  int id;
  int age;
  coord_t home;
  int wtype; // type of "workplace" (1:work, 2:school)
  int assigned;
  coord_t work;
} person_t;

typedef struct circlestep {
  int mode;
  icoord_t delta;
  int remaining;
  int rank;
} cstep_t;

// GLOBALS ---------------------------------------------------

int npeople, nstudents, nworkers;
float xmin, xmax, ymin, ymax; // village size (in meters)
float hwidth; // house width (in meters)
float xsep, ysep; // x & y house separation (center to center)
int studentAgeMin, studentAgeMax;
int workerAgeMin, workerAgeMax;
float schoolAttendanceRatio, employmentRatio;

char hhsizeFile[MAXPATHLEN];
pdf_t hhsizePDF; // household size
char ageFile[MAXPATHLEN];
pdf_t agePDF; // age distribution

list<loc_t> houses;
list<loc_t> schools;
list<loc_t> workplaces;
list<person_t> people;

int wpSize; // workplace size
int sSize; // school size
float bmargin; // additional margin when creating workplaces/schools
               // (create this many extras)
float bldgSpaceFactor; // used as exponential spacing term
float bdTol;  // building distance tolerance (use when placing buildings)

int doUniform=0;

// FUNCTIONS -------------------------------------------------

// floating point random value between 0 and 1
#define FRAND1 ((float)random()/RAND_MAX)
// floating point random between (min) and (max)
#define FRAND(min, max) (FRAND1*((max)-(min))+(min))
// integer random between (min) and (max)
#define IRAND(min, max) ((random()%((max)-(min))+(min)))

// distance between coord_t
#define CDIST(c1, c2) (sqrt(pow(c1.x-c2.x,2.)+pow(c1.y-c2.y,2.)))

void read_pdf(const char *fname, pdf_t *pdf){
  FILE *fp=NULL;
  int maxalloc=10;
  int x;  // x value
  float p; // PDF value
  int minset=0;
  int medset=0;

  bzero(pdf, sizeof(pdf_t));
  if (NULL == (pdf->val = (float*) malloc(maxalloc*sizeof(float))))
    err(errno, "failed to malloc pdf");
  if (NULL == (fp=fopen(fname, "r")))
    err(errno, "failed to open file '%s'", fname);

  while (2==fscanf(fp, "%d %f", &x, &p)){
    if (!minset){
      pdf->min = x;
      minset = 1;
    }
    if (!medset && p>0.5){
      pdf->median = x;
      medset = 1;
    }
    if (pdf->len >= maxalloc){
      maxalloc+=10;
      if (NULL==(pdf->val = (float*)realloc(pdf->val, maxalloc*sizeof(float))))
	err(errno, "failed to realloc pdf");
    }
    pdf->max = x;
    pdf->val[pdf->len++] = p;
  }
  fclose(fp);

  // sanity check
  if (pdf->max != (pdf->min + pdf->len - 1)){
    errx(EINVAL, "PDF range of values does not line up (min=%d, max=%d, len=%d)",
	 pdf->min, pdf->max, pdf->len);
  }
}

int get_val_from_pdf(pdf_t *pdf){
  float r;
  int i;
  r = FRAND1;
  for (i=0; i<pdf->len; i++){
    if (r<pdf->val[i]) return (i+pdf->min);
  }
  return pdf->max;
}

// result is in meters
float get_commute_dist_ideal(void){
  float lambda=-0.348920; // extracted from Thailand survey data
  return 1000 * log(1. - ((float)random())/RAND_MAX) / lambda;
}


void read_config(const char *fname){
  FILE *fp;

  if (NULL == (fp=fopen(fname, "r")))
    err(errno, "failed to open file '%s'", fname);

  fscanf(fp, "%s", hhsizeFile);
  fscanf(fp, "%s", ageFile);
  if (1!=fscanf(fp, "%d", &npeople))
    errx(EIO, "failed to read npeople from %s", fname);
  if (3!=fscanf(fp, "%d %d %f", &studentAgeMin, &studentAgeMax, &schoolAttendanceRatio))
    errx(EIO, "failed to read student details from %s", fname);
  if (3!=fscanf(fp, "%d %d %f", &workerAgeMin, &workerAgeMax, &employmentRatio))
    errx(EIO, "failed to read worker details from %s", fname);
  if (doUniform){
    if (3!=fscanf(fp, "%f %f %f", &hwidth, &xsep, &ysep))
      errx(EIO, "failed to read house details from %s", fname);
  } else {
    if (2!=fscanf(fp, "%f %f", &hwidth, &xsep))
      errx(EIO, "failed to read house details from %s", fname);
    ysep = xsep;
  }
  if (5!=fscanf(fp, "%d %d %f %f %f", &wpSize, &sSize, &bldgSpaceFactor, &bmargin, &bdTol))
    errx(EIO, "failed to read building info from %s", fname);

  // sanity checks
  if (hwidth > xsep || hwidth > ysep)
    errx(ENODATA, "house width (%f) is greater than house separation (%f, %f)", hwidth, xsep, ysep);

  fclose(fp);

}

void get_coord_random(coord_t *c){
  c->x = FRAND(xmin, xmax);
  c->y = FRAND(ymin, ymax);
}

void get_coord_near(coord_t *c1, coord_t *c2, float maxdist){
  float dist=maxdist+1.;
  while (dist>maxdist){
    c2->x = FRAND(c1->x-maxdist/2, c1->x+maxdist/2);
    c2->y = FRAND(c1->y-maxdist/2, c1->y+maxdist/2);
    dist = sqrt(pow(c1->x - c2->x,2.) + pow(c1->y - c2->y,2.));
  }
}

void dump_people(void){
  list<person_t>::iterator pIter;
  const char *fname;
  FILE *fp;

  fname = "people.dat";
  if (NULL==(fp=fopen(fname, "w")))
    err(errno, "failed to open %s", fname);
  fprintf(fp, "%ld\n", people.size());
  for (pIter=people.begin(); pIter!=people.end(); pIter++){
    fprintf(fp, "%d %d %f %f %f %f %f\n",
	    pIter->id,
	    pIter->age,
	    pIter->home.x, pIter->home.y,
	    pIter->work.x, pIter->work.y,
	    CDIST(pIter->home, pIter->work));
  }
  fclose(fp);
}

void snapshot_people(void){
  list<person_t>::iterator pIter;

  for (pIter=people.begin(); pIter!=people.end(); pIter++){
    fprintf(stdout, "%d %d %f %f %f %f %f\n",
	    pIter->id,
	    pIter->age,
	    pIter->home.x, pIter->home.y,
	    pIter->work.x, pIter->work.y,
	    CDIST(pIter->home, pIter->work));
  }
}

// initialize a circular step iterator
void init_csIter(cstep_t *state){
  state->mode = 0;
  state->delta.x = 0;
  state->delta.y = 1;
  state->remaining = 1;
  state->rank = 1;
}

// take a circular step
void take_cstep(icoord_t *cursor, cstep_t *state){
  cursor->x += state->delta.x;
  cursor->y += state->delta.y;

  if (! (--state->remaining)){
    state->mode = (++(state->mode))%4;
    switch(state->mode){
      case 0:{ // up
	state->delta.x = 0;
	state->delta.y = 1;
	state->rank++;
	break;
      }
      case 1:{ // right
	state->delta.x = 1;
	state->delta.y = 0;
	break;
      }
      case 2:{ // down
	state->delta.x = 0;
	state->delta.y = -1;
	state->rank++;
	break;
      }
      case 3:{ // left
	state->delta.x = -1;
	state->delta.y = 0;
	break;
      }
    }
    state->remaining = state->rank;
  }
}

void get_next_building_cstep(icoord_t &cursor, int np){
  static int csIter_init=0;
  static cstep_t csIter;

  if (!csIter_init){
    init_csIter(&csIter);
    bzero(&cursor, sizeof(cursor));
    csIter_init=1;

    return; // just use (0,0) as a start for everyone...
  }

  while (1){
    take_cstep(&cursor, &csIter);
    if (FRAND1 < exp(-bldgSpaceFactor * np / npeople)) break;
  }

  return;
}

void take_uni_step(icoord_t *cursor, icoord_t *nh){
  cursor->x += 1;

  if (cursor->x > nh->x/2){
    cursor->x = -nh->x/2;
    cursor->y -= 1;
  }
}

void get_next_building_uniform(icoord_t &cursor){
  static int uniform_init=0;
  static icoord_t nh;
  static coord_t ratio;
  int nhouses;

  if (!uniform_init){
    ratio.x = ratio.y = 1.;
    if (xsep > ysep) ratio.x = xsep / ysep; // ratio is either 1 or >1
    else             ratio.y = ysep / xsep;

    nhouses = npeople / (hhsizePDF.median * bldgSpaceFactor); // just an estimate, space factor is [0,1]
    nh.x = sqrt(nhouses * ratio.y / ratio.x); 
    nh.y = sqrt(nhouses * ratio.x / ratio.y);

    cursor.x = -nh.x/2;
    cursor.y = nh.y/2 + 1;

    uniform_init=1;
  }

  while (1){
    take_uni_step(&cursor, &nh);
    if (FRAND1 < bldgSpaceFactor) break;
  }

  return;
}

void make_buildings(void){
  loc_t bldg;
  int np=0;
  int id=0;
  icoord_t cursor;

  houses.clear();
  schools.clear();
  workplaces.clear();  

  while (houses.size() * hhsizePDF.median < npeople){
    if (doUniform)
      get_next_building_uniform(cursor);
    else
      get_next_building_cstep(cursor, np);

    bldg.id = id++;
    bldg.coord.x = cursor.x * (xsep);
    bldg.coord.y = cursor.y * (ysep);
    
    if (workplaces.size()*wpSize*(1.+bmargin) < np){ // need more workplaces
      bldg.type = 2;
      bldg.capacity = wpSize;
      workplaces.push_back(bldg);
    } else if (schools.size()*sSize*(1.+bmargin) < np) { // need more schools
      bldg.type = 3;
      bldg.capacity = sSize;
      schools.push_back(bldg);
    } else { // make a house
      bldg.type = 1;
      bldg.capacity = get_val_from_pdf(&hhsizePDF);
      np += bldg.capacity;
      houses.push_back(bldg); // store this in the list
    }

    if (bldg.coord.x < xmin) xmin = bldg.coord.x;
    if (bldg.coord.x > xmax) xmax = bldg.coord.x;
    if (bldg.coord.y < ymin) ymin = bldg.coord.y;
    if (bldg.coord.y > ymax) ymax = bldg.coord.y;
  }

#ifdef UNDEF
  float scf = 1.25; // scale-up the extrema
  xmin *= scf;
  xmax *= scf;
  ymin *= scf;
  ymax *= scf;
#endif
}

void make_people(void){
  int i;
  list<loc_t>::iterator lIter;
  person_t prsn;

  people.clear();

  // walk your way through the houses, populating them with people
  for (lIter=houses.begin(); lIter!=houses.end(); lIter++){
    for (i=0; i<lIter->capacity; i++){
      bzero(&prsn, sizeof(prsn));

      // ID #
      prsn.id = people.size();

      // assign an age
      if (i)
	prsn.age = get_val_from_pdf(&agePDF); // any age will do
      else
	for (prsn.age=0; prsn.age<workerAgeMin; prsn.age=get_val_from_pdf(&agePDF)); // forced adult

      // assign a home (smear over house size)
      prsn.home.x = FRAND(lIter->coord.x - hwidth/2, lIter->coord.x + hwidth/2);
      prsn.home.y = FRAND(lIter->coord.y - hwidth/2, lIter->coord.y + hwidth/2);

      if (prsn.age >= studentAgeMin &&
	  prsn.age <= studentAgeMax && 
	  FRAND1 <= schoolAttendanceRatio){
	prsn.wtype = 2; // student
      } else if (prsn.age >= workerAgeMin &&
		 prsn.age <= workerAgeMax &&
		 FRAND1 <= employmentRatio){
	prsn.wtype = 1; // worker
      } else {
	prsn.wtype = 0; // homebody...
      }
      people.push_back(prsn);
    }
  }
}

void dump_buildings(void){
  list<loc_t>::iterator lIter;
  FILE *fp;
  const char *fname;
  
  fname = "locations.dat";
  if (NULL==(fp=fopen(fname, "w")))
    err(errno, "failed to open %s", fname);
  fprintf(fp, "%f %f %f\n", hwidth, xsep, ysep);
  fprintf(fp, "%f %f %f %f\n", xmin, xmax, ymin, ymax);
  fprintf(fp, "%ld\n", houses.size() + workplaces.size() + schools.size());
  for (lIter=houses.begin(); lIter!=houses.end(); lIter++){
    fprintf(fp, "%d %d %f %f %d %d\n", 
	    lIter->id,
	    lIter->type,
	    lIter->coord.x,
	    lIter->coord.y,
	    lIter->capacity,
	    lIter->openings);
  }
  for (lIter=workplaces.begin(); lIter!=workplaces.end(); lIter++){
    fprintf(fp, "%d %d %f %f %d %d\n", 
	    lIter->id,
	    lIter->type,
	    lIter->coord.x,
	    lIter->coord.y,
	    lIter->capacity,
	    lIter->openings);
  }
  for (lIter=schools.begin(); lIter!=schools.end(); lIter++){
    fprintf(fp, "%d %d %f %f %d %d\n", 
	    lIter->id,
	    lIter->type,
	    lIter->coord.x,
	    lIter->coord.y,
	    lIter->capacity,
	    lIter->openings);
  }
  fclose(fp);
}

void select_workplaces(void){
  list<loc_t>::iterator lIter;
  list<person_t>::iterator pIter;
  loc_t *ltgt;
  float d, dmin, dmax;

  // reset the capacities
  for (lIter=workplaces.begin(); lIter!=workplaces.end(); lIter++){
    lIter->openings = lIter->capacity;
  }
  for (lIter=schools.begin(); lIter!=schools.end(); lIter++){
    lIter->openings = lIter->capacity;
  }

  for (pIter=people.begin(); pIter!=people.end(); pIter++){
    pIter->assigned=0;
    switch (pIter->wtype){
      case 1:{// worker, needs a workplace
	while (! pIter->assigned){ // keep trying until you find one that fits
	  dmin = get_commute_dist_ideal();
	  dmax = dmin * (1.+bdTol);
	  dmin *= (1.-bdTol);
	  for (lIter=workplaces.begin(); lIter!=workplaces.end(); lIter++){
	    if (0<lIter->openings){
	      d = CDIST(pIter->home, lIter->coord);
	      if (d >= dmin && d <= dmax){
		// add the worker to the workplace
		pIter->work.x = lIter->coord.x;
		pIter->work.y = lIter->coord.y;
		pIter->assigned = 1;
		lIter->openings--;
		
		//	      printf("worker %d: workplace = %d (@ %f m), %d openings remain\n",
		//		     pIter->id, lIter->id, dmin, lIter->openings);
		break;
	      }
	    }
	  }
	}
	break;
      }
      case 2:{ // student, needs a school
	dmin = 1.e10; // something big to start
	ltgt = NULL;
	for (lIter=schools.begin(); lIter!=schools.end(); lIter++){
	  if (lIter->openings){
	    d = CDIST(pIter->home, lIter->coord);
	    if (d < dmin){
	      dmin = d;
	      ltgt = &(*lIter);
	    }
	  }
	}
	if (ltgt){
	  // add the student to this school
	  pIter->work.x = ltgt->coord.x;
	  pIter->work.y = ltgt->coord.y;
	  pIter->assigned = 1;
	  ltgt->openings--;
	  
	  //	printf("student %d: closest school = %d (@ %f m), %d openings remain\n",
	  //	       pIter->id, ltgt->id, dmin, ltgt->openings);
	  
	} else {
	  errx(ENODATA, "no school selected for ID %d", pIter->id);
	}
	break;
      }
      default:{ // homebody, neither work nor school
	pIter->work.x = pIter->home.x;
	pIter->work.y = pIter->home.y;
	break;
      }
    } // switch
  } // loop over people
}

// MAIN ---------------------------------------------------------

int main (int argc, char **argv){
  int i, j;
  int c;
  char configFile[MAXPATHLEN];
  char outputFile[MAXPATHLEN];

  strncpy(configFile, "mkinput.cfg", MAXPATHLEN);
  strncpy(outputFile, "Dengue.in", MAXPATHLEN);

  while (-1 != (c = getopt(argc, argv, "c:o:ur:"))){
    switch (c) {
      case 'c':{
	strncpy(configFile, optarg, MAXPATHLEN);
	break;
      }
      case 'o':{
	strncpy(outputFile, optarg, MAXPATHLEN);
	break;
      }
      case 'u':{
	doUniform=1;
	break;
      }
      case 'r':{
	srandom(atoi(optarg));
	break;
      }
      default:{
	fprintf(stderr, "Usage: %s [options]\n"
"valid options:\n"
" -c <config_file>      Configuration file used by mkinput\n"
" -o <output_file>      Output file, to be used as input to Dengue model\n"
" -r <random_seed>      Pre-Initialize the random seed\n"
" -u                    Lay out buildings on a Uniform rectangular grid\n"
		, argv[0]);
	exit(1);
      }
    }
  }

  // read input files
  read_config(configFile);
  read_pdf(hhsizeFile, &hhsizePDF);
  read_pdf(ageFile, &agePDF);

  // count students, based statistically on the age PDF
  i = studentAgeMin - agePDF.min + 1;
  j = studentAgeMax - agePDF.min + 1;
  nstudents = npeople * (agePDF.val[j] - agePDF.val[i]) * schoolAttendanceRatio;

  // count workers, based statistically on the age PDF
  i = workerAgeMin - agePDF.min + 1;
  j = workerAgeMax - agePDF.min + 1;
  nworkers = npeople * (agePDF.val[j] - agePDF.val[i]) * employmentRatio;

  // make houses / workplaces / schools
  make_buildings();
  make_people();

  // match up people with workplaces/schools
  select_workplaces();

  dump_buildings();
  dump_people();

  return 0;
}
