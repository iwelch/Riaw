
#include <math.h>
#define MAXLATLONID (361*181) + 1

static int mklatlonid( const int lat, const int lon ) {
  assert( abs(lat) <= 90 );
  assert( abs(lon) <= 180 );
  return (lat+90)*360 + (lon+180) + 1;
}

static int latlon[2]; // pick off immediately!! does not survive reentry

static int *invlatlon( const int latlonid ) {
  assert( latlonid > 0 ); // start at 1!!
  assert( latlonid < MAXLATLONID ); // extremely unlikely to work around pole
  latlon[0] = (latlonid-1)/360 - 90;
  latlon[1] = (latlonid-1)%360 - 180;
  return latlon;
}
