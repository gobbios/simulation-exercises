data {
  int<lower=0> Ngroups;
  int<lower=0> Ncases;
  vector[Ngroups] grouplevelresponse;
  int Ncasespergroup[Ngroups];
  vector[Ncases] subjectlevelpred;
  matrix[Ncases, Ngroups] locmat;
}
parameters {
  real intercept;
  real slope;
  real<lower=0> sigma;
  vector<lower=0>[Ngroups] g_sigma;
  vector[Ngroups] g_meanval;
}
model {
  for (n in 1:Ngroups) {
    // get subject-level data for the predictor
    vector[Ncasespergroup[n]] tempdata;
    int pos;
    pos = 1;
    for (i in 1:rows(locmat)) {
      if (locmat[i, n]) {
        tempdata[pos] = subjectlevelpred[i];
        pos = pos + 1;
      }
    }
    // model group level predictor
    //
    g_sigma[n] ~ normal(1, 3);
    g_meanval[n] ~ normal(tempdata, g_sigma[n]);
    // the actual model on group level
    grouplevelresponse[n] ~ normal(intercept + slope * g_meanval[n], sigma);
  }
}
