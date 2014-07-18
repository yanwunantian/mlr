#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

static void print_vector(double *x, int n) {
  for (int i = 0; i < n; ++i) {
    Rprintf("%2.2f ", x[i]);
  }
  Rprintf("\n");
}

double get_min(double *x, int n) {
  double xmin = DBL_MAX;
  for (int i = 0; i < n; i++) {
    if (x[i] < xmin)
      xmin = x[i];
  }
  return(xmin);
}

SEXP cstree_split(SEXP s_y, SEXP s_wt, SEXP s_x, SEXP s_parms, SEXP s_continuous) {
  SEXP s_goodness, s_res;
  /*
  n = nrow(y)
  goodness = numeric(n-1)
  if (continuous) {
    yy0 = colSums(y[1:n,,drop=FALSE] * wt[1:n])
    for (i in 1:(n-1)) {
      yy1 = colSums(y[1:i,,drop=FALSE] * wt[1:i])
      yy2 = colSums(y[(i+1):n,,drop=FALSE] * wt[(i+1):n])
      goodness[i] = min(yy0) - min(yy1) - min(yy2)
    }
    # FIXME direction
    list(goodness = goodness, direction = rep(-1, n-1))
}
  */
  const R_len_t n = nrows(s_y);
  const R_len_t m = ncols(s_y);
  double *y = REAL(s_y);
  int i, j, split;
  PROTECT(s_goodness = allocVector(REALSXP, n-1));
  double *goodness = REAL(s_goodness);
  double sum1[m], sum2[m];
  double loss0, loss1, loss2;

  //GetRNGstate();
  //double xx = runif(0, 1);
  //Rprintf("%f\n", xx);
  //PutRNGstate();
  // FIXME
  //if (xx > 0.66) {
  //  for (split = 0; split < n-1; split++)
  //    goodness[split] = 0;
  //} else {
  for (int j=0; j < m; j++) {
    sum1[j] = 0; sum2[j] = 0;
  }

  for (j = 0; j < m; j++) {
   for (i = 0; i < n; i++) {
     sum2[j] += y[j*n + i];
    }
  }
  loss0 = get_min(sum2, m);
  //Rprintf("%f\n", loss0);
  //print_vector(sum1, m) ;
  for (split = 0; split < n-1; split++) {
    for (j = 0; j < m; j++) {
      sum1[j] += y[j*n + split];
      sum2[j] -= y[j*n + split];
    }
    goodness[split] = loss0 - get_min(sum1, m) - get_min(sum2, m);
  }

  PROTECT(s_res = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(s_res, 0, s_goodness);
  SET_VECTOR_ELT(s_res, 1, ScalarInteger(7));
  UNPROTECT(2); /* s_goodness, s_res */
  return s_res;
}
