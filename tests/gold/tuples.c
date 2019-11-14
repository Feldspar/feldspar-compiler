#include "tuples.h"


void tuples(int32_t v0, int32_t * out)
{
  int32_t v1;
  int32_t v7;
  int32_t v14;
  int32_t v24;
  int32_t v6;
  int32_t v15;
  int32_t v23;
  int32_t v35;
  int32_t v13;
  int32_t v25;
  int32_t v34;
  int32_t v51;
  int32_t v67;
  int32_t v36;
  int32_t v22;
  int32_t v33;
  int32_t v50;
  int32_t v37;
  int32_t v49;
  int32_t v66;
  int32_t v48;
  int32_t v65;
  int32_t v47;
  int32_t v64;
  int32_t v46;
  int32_t v63;
  int32_t v62;
  int32_t v61;
  
  v1 = (v0 * 3);
  v7 = (v0 + v1);
  v14 = (v1 + v7);
  v24 = (v14 + v1);
  v6 = (v1 + v0);
  v15 = (v6 + v1);
  v23 = (v15 + v14);
  v35 = (v23 + v24);
  v13 = (v7 + v6);
  v25 = (v13 + v15);
  v34 = (v25 + v23);
  v51 = (v34 + v35);
  v67 = (v1 + v51);
  v36 = (v1 + v25);
  v22 = (v1 + v13);
  v33 = (v22 + v1);
  v50 = (v33 + v36);
  v37 = (v24 + v22);
  v49 = (v37 + v33);
  v66 = (v49 + v50);
  v48 = (v1 + v34);
  v65 = (v48 + v1);
  v47 = (v35 + v37);
  v64 = (v47 + v49);
  v46 = (v36 + v1);
  v63 = (v46 + v48);
  v62 = (v51 + v47);
  v61 = (v50 + v46);
  *out = ((((((((((((((v62 + v64) + v66) + v61) + v63) + v65) + v67) + v62) + v64) + v66) + v61) + v63) + v65) + v67) + (v1 * v51));
}
