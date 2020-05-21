#include "tuples.h"


void tuples(int32_t v0, int32_t * out)
{
  int32_t v1;
  int32_t v7;
  int32_t v6;
  int32_t v15;
  int32_t v25;
  int32_t v14;
  int32_t v22;
  int32_t v36;
  int32_t v33;
  int32_t v48;
  int32_t v13;
  int32_t v24;
  int32_t v35;
  int32_t v46;
  int32_t v68;
  int32_t v23;
  int32_t v34;
  int32_t v49;
  int32_t v67;
  int32_t v37;
  int32_t v51;
  int32_t v47;
  int32_t v66;
  int32_t v50;
  int32_t v65;
  int32_t v64;
  int32_t v63;
  int32_t v61;
  
  v1 = (v0 * 3);
  v7 = (v0 + v1);
  v6 = (v1 + v0);
  v15 = (v7 + v6);
  v25 = (v1 + v15);
  v14 = (v1 + v7);
  v22 = (v14 + v1);
  v36 = (v22 + v25);
  v33 = (v25 + v1);
  v48 = (v36 + v33);
  v13 = (v6 + v1);
  v24 = (v15 + v13);
  v35 = (v1 + v24);
  v46 = (v33 + v35);
  v68 = (v48 + v46);
  v23 = (v13 + v14);
  v34 = (v24 + v23);
  v49 = (v1 + v34);
  v67 = (v49 + v1);
  v37 = (v23 + v22);
  v51 = (v37 + v36);
  v47 = (v34 + v37);
  v66 = (v47 + v51);
  v50 = (v35 + v1);
  v65 = (v46 + v50);
  v64 = (v51 + v48);
  v63 = (v50 + v49);
  v61 = (v1 + v47);
  *out = ((((((((((((((v66 + v64) + v68) + v65) + v63) + v67) + v61) + v66) + v64) + v68) + v65) + v63) + v67) + v61) + (v1 * v47));
}
